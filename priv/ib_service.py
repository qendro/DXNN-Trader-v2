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
    pass

# Logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Global state
ib = IB()
running = True

# Remember last connect params for reconnection
_last_connect = {"host": "host.docker.internal", "port": 7497, "client_id": 101}

# ---------------- I/O ----------------

async def read_msg():
    """Read length-prefixed message from Erlang"""
    try:
        hdr = await asyncio.get_running_loop().run_in_executor(None, sys.stdin.buffer.read, 4)
        if not hdr:
            return None
        (n,) = struct.unpack('>I', hdr)
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
    logger.info(msg % args if args else msg)

def send_error(cid, code, message):
    write_msg({"v": 1, "type": "error", "cid": cid, "code": code, "message": message})

def validate_message(msg):
    return isinstance(msg, dict) and all(k in msg for k in ("type", "cid"))

def n(x):
    """NaN-safe helper: NaN→None for JSON"""
    return None if x is None or x != x else x

# ---------------- Symbol helpers ----------------

def parse_symbol(sym: str) -> Forex:
    """'EUR.USD' → Forex('EURUSD'); pass-through if already EURUSD"""
    if '.' in sym:
        base, quote = sym.split('.', 1)
        return Forex(base + quote)
    return Forex(sym)

def format_symbol_for_output(contract_symbol: str) -> str:
    """EURUSD → EUR.USD (preferred by Erlang side)"""
    if len(contract_symbol) == 6:
        return f"{contract_symbol[:3]}.{contract_symbol[3:]}"
    if len(contract_symbol) == 3:
        return f"{contract_symbol}.USD"
    return contract_symbol

# ---------------- Main loop ----------------

async def main():
    global running
    log_info("Python bridge starting up")
    try:
        while running:
            msg = await read_msg()
            if msg is None:  # EOF
                log_info("Received EOF, shutting down")
                break
            if not validate_message(msg):
                send_error(None, "BAD_REQ", "Missing required fields")
                continue
            await handle_command(msg)
    except KeyboardInterrupt:
        log_info("Received interrupt, shutting down")
    finally:
        if ib.isConnected():
            ib.disconnect()
        log_info("Bridge shutdown complete")

async def handle_command(cmd):
    t = cmd.get('type')
    cid = cmd.get('cid')
    try:
        if t == 'connect':
            await handle_connect(cmd, cid)
        elif t == 'subscribe':
            await handle_subscribe(cmd, cid)
        elif t == 'place_order':
            await handle_place_order(cmd, cid)
        else:
            send_error(cid, "BAD_REQ", f"Unknown command type: {t}")
    except Exception as e:
        log_info("Error in handle_command: %s", str(e))
        send_error(cid, "BRIDGE_IO", str(e))

# ---------------- Commands ----------------

async def handle_connect(cmd, cid):
    """Connect with paper-trading enforcement"""
    host = cmd.get('host', _last_connect["host"])
    port = cmd.get('port', _last_connect["port"])
    client_id = cmd.get('client_id', _last_connect["client_id"])

    # Paper-only guard
    if port != 7497 and not os.getenv('ALLOW_LIVE'):
        send_error(cid, "IB_REJECT", "Paper only (port 7497)")
        return

    try:
        log_info("Connecting to IB %s:%d (client_id=%d)", host, port, client_id)
        await ib.connectAsync(host, port, clientId=client_id, timeout=10)
        ib.reqMarketDataType(3)  # delayed data for paper
        write_msg({"v": 1, "type": "connected", "cid": cid})
        log_info("Connected to IB successfully")

        # store for reconnection
        _last_connect.update({"host": host, "port": port, "client_id": client_id})

        # background tasks
        asyncio.create_task(heartbeat())
        asyncio.create_task(connection_monitor())
    except Exception as e:
        log_info("Connection failed: %s", str(e))
        send_error(cid, "IB_CONN", str(e))

async def handle_subscribe(cmd, cid):
    symbol = cmd.get('symbol', 'EUR.USD')
    try:
        log_info("Subscribing to market data for %s", symbol)
        contract = parse_symbol(symbol)
        ib.reqMktData(contract)
        write_msg({"v": 1, "type": "subscribed", "cid": cid, "symbol": symbol})
    except Exception as e:
        log_info("Subscription failed for %s: %s", symbol, str(e))
        send_error(cid, "IB_REJECT", str(e))

async def handle_place_order(cmd, cid):
    """Place order and forward fills via Trade.updateEvent"""
    symbol = cmd.get('symbol')
    action = cmd.get('action')        # 'BUY' or 'SELL'
    quantity = cmd.get('quantity')
    order_type = cmd.get('order_type', 'MKT')

    if not all([symbol, action, quantity]):
        send_error(cid, "BAD_REQ", "Missing required order parameters: symbol, action, quantity")
        return

    if not os.getenv('ALLOW_LIVE_ORDERS'):
        log_info("Order placement: Paper trading mode enforced")

    try:
        log_info("Placing %s order: %s %s %s", order_type, action, quantity, symbol)

        contract = parse_symbol(symbol)
        order = MarketOrder(action, quantity)
        trade = ib.placeOrder(contract, order)

        # Send immediate ack (order submitted)
        write_msg({
            "v": 1, "type": "order_placed", "cid": cid,
            "order_id": trade.order.orderId,
            "symbol": symbol, "action": action,
            "quantity": quantity, "order_type": order_type
        })
        log_info("Order placed successfully: ID %s", trade.order.orderId)

        # --- NEW: forward fills/updates back to Erlang ---
        def on_trade_update(tr: "ib_insync.objects.Trade"):
            try:
                st = tr.orderStatus.status  # e.g., 'PreSubmitted','Submitted','Filled','PartiallyFilled'
                payload = {
                    "v": 1,
                    "type": "order_status",
                    "order_id": tr.order.orderId,
                    "symbol": format_symbol_for_output(tr.contract.symbol),
                    "action": tr.order.action,
                    "status": st,
                    "filled": n(tr.orderStatus.filled),
                    "avg_price": n(tr.orderStatus.avgFillPrice)
                }
                write_msg(payload)

                if st in ("Filled", "PartiallyFilled"):
                    # also emit a dedicated 'order_filled' for convenience
                    write_msg({
                        "v": 1, "type": "order_filled",
                        "order_id": tr.order.orderId,
                        "symbol": format_symbol_for_output(tr.contract.symbol),
                        "side": tr.order.action,
                        "filled": n(tr.orderStatus.filled),
                        "price": n(tr.orderStatus.avgFillPrice)
                    })
            except Exception as e:
                log_info("on_trade_update error: %s", str(e))

        trade.updateEvent += on_trade_update
        # -------------------------------------------------

    except Exception as e:
        log_info("Order placement failed for %s %s %s: %s", action, quantity, symbol, str(e))
        send_error(cid, "IB_REJECT", str(e))

# ---------------- Background tasks ----------------

async def heartbeat():
    while running:
        try:
            write_msg({"v": 1, "type": "beat", "ts": int(time.time() * 1000), "tws_ok": ib.isConnected()})
        except Exception as e:
            log_info("Heartbeat error: %s", str(e))
        await asyncio.sleep(3)

async def connection_monitor():
    reconnect_attempts = 0
    max_attempts = 5
    while running:
        try:
            if not ib.isConnected() and reconnect_attempts < max_attempts:
                log_info("Connection lost, attempting reconnection (%d/%d)", reconnect_attempts + 1, max_attempts)
                write_msg({"v": 1, "type": "resync", "phase": "start"})
                try:
                    await ib.connectAsync(_last_connect["host"], _last_connect["port"],
                                          clientId=_last_connect["client_id"], timeout=10)
                    ib.reqMarketDataType(3)
                    write_msg({"v": 1, "type": "resync", "phase": "done"})
                    log_info("Reconnection successful")
                    reconnect_attempts = 0
                except Exception as e:
                    reconnect_attempts += 1
                    log_info("Reconnection attempt failed: %s", str(e))
                    send_error(None, "IB_CONN", f"Reconnect failed: {str(e)}")
            elif reconnect_attempts >= max_attempts:
                log_info("Max reconnection attempts reached, giving up")
                write_msg({"v": 1, "type": "resync", "phase": "failed"})
                break
        except Exception as e:
            log_info("Connection monitor error: %s", str(e))
        await asyncio.sleep(5)

# ---------------- Tick forwarding ----------------

def on_pending_tickers(tickers):
    try:
        for t in tickers:
            if t.bid is None and t.ask is None and t.last is None:
                continue
            write_msg({
                "v": 1, "type": "tick",
                "symbol": format_symbol_for_output(t.contract.symbol),
                "bid": n(t.bid), "ask": n(t.ask), "last": n(t.last),
                "volume": n(t.volume)
            })
    except Exception as e:
        log_info("Tick processing error: %s", str(e))

ib.pendingTickersEvent += on_pending_tickers

# ---------------- Entrypoint ----------------

if __name__ == '__main__':
    asyncio.run(main())
