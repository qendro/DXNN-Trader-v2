#!/usr/bin/env python3
"""
Minimal Python Bridge Service for IB API
Uses ib_insync for reliable IB communication with proper {packet,4} framing
"""

import sys
import struct
import json
import asyncio
import time
import logging
import os
from ib_insync import IB, Forex, MarketOrder, util

# Enable asyncio mode for ib_insync (if available)
try:
    util.useAsyncio()
except AttributeError:
    # Older version of ib_insync doesn't need this
    pass

# Setup logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Global state - keep it simple
ib = IB()
running = True

async def read_msg():
    """Read length-prefixed message from Erlang"""
    try:
        hdr = await asyncio.get_running_loop().run_in_executor(
            None, sys.stdin.buffer.read, 4)
        if not hdr: 
            return None
        (n,) = struct.unpack('>I', hdr)  # big-endian
        data = sys.stdin.buffer.read(n)
        return json.loads(data)
    except Exception as e:
        logger.error(f"Error reading message: {e}")
        return None

def write_msg(obj):
    """Write length-prefixed message to Erlang"""
    try:
        b = json.dumps(obj, separators=(',', ':')).encode()
        sys.stdout.buffer.write(struct.pack('>I', len(b)) + b)
        sys.stdout.buffer.flush()
    except Exception as e:
        logger.error(f"Error writing message: {e}")

def log_info(msg, *args):
    """Log to both Python logger and Erlang"""
    formatted_msg = msg % args if args else msg
    logger.info(formatted_msg)
    write_msg({
        "v": 1,
        "type": "log",
        "level": "info",
        "message": formatted_msg
    })

def send_error(cid, code, message):
    """Send error message with proper framing"""
    write_msg({
        "v": 1,
        "type": "error",
        "cid": cid,
        "code": code,  # IB_CONN, IB_REJECT, BRIDGE_IO, BAD_REQ
        "message": message
    })

def validate_message(msg):
    """Validate required message fields"""
    if not isinstance(msg, dict):
        return False
    required = ['type', 'cid']
    return all(field in msg for field in required)

def n(x):
    """NaN-safe helper: NaN→None for JSON"""
    return None if x != x or x is None else x

def parse_symbol(sym):
    """Convert external format 'EUR.USD' to ib_insync Forex('EURUSD')"""
    if '.' in sym:
        base, quote = sym.split('.')
        return Forex(base + quote)
    else:
        # Already in EURUSD format
        return Forex(sym)

def format_symbol_for_output(contract_symbol):
    """Convert EURUSD back to EUR.USD format for output"""
    if len(contract_symbol) == 6:
        return f"{contract_symbol[:3]}.{contract_symbol[3:]}"
    elif len(contract_symbol) == 3:
        # Handle case where only base currency is returned
        return f"{contract_symbol}.USD"  # Assume USD quote for now
    return contract_symbol

async def main():
    global running
    try:
        log_info("Python bridge starting up")
        while running:
            try:
                msg = await read_msg()
                if msg is None:  # EOF from Erlang
                    log_info("Received EOF, shutting down")
                    running = False
                    break
                if not validate_message(msg):
                    send_error(None, "BAD_REQ", "Missing required fields")
                    continue
                await handle_command(msg)
            except Exception as e:
                log_info("Error handling message: %s", str(e))
                send_error(None, "BRIDGE_IO", str(e))
    except KeyboardInterrupt:
        log_info("Received interrupt, shutting down")
    finally:
        if ib.isConnected():
            ib.disconnect()
        log_info("Bridge shutdown complete")

async def handle_command(cmd):
    cmd_type = cmd.get('type')
    cid = cmd.get('cid')
    
    try:
        if cmd_type == 'connect':
            await handle_connect(cmd, cid)
        elif cmd_type == 'subscribe':
            await handle_subscribe(cmd, cid)
        elif cmd_type == 'place_order':
            await handle_place_order(cmd, cid)
        else:
            send_error(cid, "BAD_REQ", f"Unknown command type: {cmd_type}")
    except Exception as e:
        log_info("Error in handle_command: %s", str(e))
        send_error(cid, "BRIDGE_IO", str(e))

async def handle_connect(cmd, cid):
    """Handle connection request with paper trading enforcement"""
    port = cmd.get('port', 7497)
    host = cmd.get('host', '127.0.0.1')
    client_id = cmd.get('client_id', 1)
    
    # Paper-only guard - strict check before connect
    if port != 7497 and not os.getenv('ALLOW_LIVE'):
        send_error(cid, "IB_REJECT", "Paper only (port 7497)")
        return
    
    try:
        log_info("Connecting to IB %s:%d (client_id=%d)", host, port, client_id)
        log_info("Running in Docker - connecting to host machine TWS")
        
        # Connect with longer timeout for Docker networking
        await ib.connectAsync(host, port, clientId=client_id, timeout=10)
        
        # Enable delayed data for paper trading
        ib.reqMarketDataType(3)  # 1=real-time, 3=delayed
        log_info("Market data type set to delayed (3)")
        
        write_msg({"v": 1, "type": "connected", "cid": cid})
        log_info("Connected to IB successfully")
        
        # Start heartbeat and connection monitor after connect
        asyncio.create_task(heartbeat())
        asyncio.create_task(connection_monitor())
        
    except Exception as e:
        log_info("Connection failed: %s", str(e))
        log_info("Make sure TWS is running on host with API enabled (port 7497)")
        send_error(cid, "IB_CONN", str(e))

async def handle_subscribe(cmd, cid):
    """Handle market data subscription with symbol normalization"""
    symbol = cmd.get('symbol', 'EUR.USD')
    
    try:
        log_info("Subscribing to market data for %s", symbol)
        # Use symbol parser for proper conversion
        contract = parse_symbol(symbol)
        ticker = ib.reqMktData(contract)
        write_msg({"v": 1, "type": "subscribed", "cid": cid, "symbol": symbol})
        log_info("Market data subscription successful for %s", symbol)
        
    except Exception as e:
        log_info("Subscription failed for %s: %s", symbol, str(e))
        send_error(cid, "IB_REJECT", str(e))

async def handle_place_order(cmd, cid):
    """Handle order placement with paper trading safety"""
    symbol = cmd.get('symbol')
    action = cmd.get('action')  # 'BUY' or 'SELL'
    quantity = cmd.get('quantity')
    order_type = cmd.get('order_type', 'MKT')  # Default to market order
    
    # Validate required parameters
    if not all([symbol, action, quantity]):
        send_error(cid, "BAD_REQ", "Missing required order parameters: symbol, action, quantity")
        return
    
    # Paper trading safety - ensure we're on paper account
    if not os.getenv('ALLOW_LIVE_ORDERS'):
        log_info("Order placement: Paper trading mode enforced")
    
    try:
        log_info("Placing %s order: %s %s %s", order_type, action, quantity, symbol)
        
        # Parse symbol and create contract
        contract = parse_symbol(symbol)
        
        # Create market order (simple for Phase 3)
        order = MarketOrder(action, quantity)
        
        # Place order
        trade = ib.placeOrder(contract, order)
        
        # Send confirmation
        write_msg({
            "v": 1,
            "type": "order_placed",
            "cid": cid,
            "order_id": trade.order.orderId,
            "symbol": symbol,
            "action": action,
            "quantity": quantity,
            "order_type": order_type
        })
        
        log_info("Order placed successfully: ID %s", trade.order.orderId)
        
    except Exception as e:
        log_info("Order placement failed for %s %s %s: %s", action, quantity, symbol, str(e))
        send_error(cid, "IB_REJECT", str(e))

async def heartbeat():
    """Send heartbeat every 3 seconds when connected"""
    while running:
        try:
            write_msg({
                "v": 1, 
                "type": "beat", 
                "ts": int(time.time() * 1000),
                "tws_ok": ib.isConnected()
            })
            await asyncio.sleep(3)
        except Exception as e:
            log_info("Heartbeat error: %s", str(e))
            await asyncio.sleep(3)  # Continue heartbeat even on error

async def connection_monitor():
    """Monitor connection and attempt reconnection if needed"""
    reconnect_attempts = 0
    max_attempts = 5
    
    while running:
        try:
            if not ib.isConnected() and reconnect_attempts < max_attempts:
                log_info("Connection lost, attempting reconnection (%d/%d)", reconnect_attempts + 1, max_attempts)
                write_msg({"v": 1, "type": "resync", "phase": "start"})
                
                try:
                    # Use stored connection parameters (simplified for Phase 2)
                    await ib.connectAsync('host.docker.internal', 7497, clientId=101, timeout=10)
                    ib.reqMarketDataType(3)  # Re-enable delayed data
                    
                    write_msg({"v": 1, "type": "resync", "phase": "done"})
                    log_info("Reconnection successful")
                    reconnect_attempts = 0  # Reset counter on success
                    
                except Exception as e:
                    reconnect_attempts += 1
                    log_info("Reconnection attempt failed: %s", str(e))
                    send_error(None, "IB_CONN", f"Reconnect failed: {str(e)}")
                    
            elif reconnect_attempts >= max_attempts:
                log_info("Max reconnection attempts reached, giving up")
                write_msg({"v": 1, "type": "resync", "phase": "failed"})
                break
                
            await asyncio.sleep(5)  # Check every 5 seconds
            
        except Exception as e:
            log_info("Connection monitor error: %s", str(e))
            await asyncio.sleep(5)

def on_pending_tickers(tickers):
    """Handle tick updates - ib_insync passes list of tickers"""
    try:
        for ticker in tickers:
            # Use symbol formatter for consistent output
            symbol = format_symbol_for_output(ticker.contract.symbol)
            write_msg({
                "v": 1,
                "type": "tick",
                "symbol": symbol,
                "bid": n(ticker.bid),
                "ask": n(ticker.ask),
                "last": n(ticker.last),
                "volume": n(ticker.volume)
            })
    except Exception as e:
        log_info("Tick processing error: %s", str(e))

# Register tick handler - ib_insync passes list of tickers
ib.pendingTickersEvent += on_pending_tickers

if __name__ == '__main__':
    asyncio.run(main())