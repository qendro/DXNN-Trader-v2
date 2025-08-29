modify my code to assume the following are in ib_service_config.json. Note I have consolidated the number of config parameters. historical data pulled should be same as live data about to be aggregated. 

* **ib\_connection**

  * `host`
  * `port`
  * `client_id`
  * `timeout`
  * `market_data_type` (1=real-time, 3=delayed)

* **symbols**

  * `list` of symbols to subscribe (e.g., `["EUR.USD","GBP.USD","USD.JPY"]`)
  * (optional per-symbol settings)

    * `whatToShow` (e.g., `"MIDPOINT"`, `"BID_ASK"`)
    * `bar_size` override (e.g., `"1 min"`)

* **startup**

  * `preload_on_startup` (was `AUTO_BACKFILL`)
  * `historical_preload_weeks`
  * `historical_preload_bar_size`
  * `tick_aggregation_enabled`

* **historical\_data**

  * `default_weeks` -> this should be the same as historical_preload_weeks`
  * `default_bar_size` -> this should be the same as `historical_preload_bar_size`
  * `whatToShow` (e.g., `"MIDPOINT"`) -> this should be the same as `whatToShow`
  * `useRTH` (true/false)

* **tick\_aggregation**

  * `bar_duration_seconds` (e.g., 60) -> this should be the same as `bar_size` override (e.g., `"1 min"`)
  * `price_selection` (e.g., `"mid_then_last"` | `"last_only"` | `"bid_ask_mid_only"`) -> this should be the same as `whatToShow`
  * `missing_volume_as_zero` (true/false)

* **trade\_execution**

  * `default_quantity`
  * `default_order_type` (e.g., `"MKT"`)
  * `time_in_force` (e.g., `"DAY"`, `"GTC"`)
  * `paper_mode_enforced` (default state at startup)
  * `allow_live_orders` (master switch)
  * `max_daily_trades`
  * `order_types_allowed` (e.g., `["MKT","LMT"]`) -> this should be the same as `default_order_type`
  * `limit_price_offset_bps` (for convenience when building LMT orders)

* **risk\_management**

  * `max_position_size`
  * `daily_loss_limit`
  * `max_open_orders`
  * `block_new_orders_on_error` (true/false)

* **monitoring**

  * `heartbeat_interval_seconds`
  * `connection_monitor_interval_seconds`
  * `max_reconnect_attempts`
  * `reconnect_backoff_seconds` (base backoff)


* **safety / environment\_overrides**

  * `respect_env_allow_live` (true/false) - This should be combined with below on Live or Paper... 
  * `respect_env_allow_live_orders` (true/false)
  * `allowed_live_ports` (e.g., `[7496, 7497]`)

#!/usr/bin/env python3
"""
Comprehensive Python-Centric IB Service
Handles ALL Interactive Brokers operations: connection, historical data, live streaming,
tick aggregation, and trade execution. Erlang focuses purely on neural networks.

Architecture:
- IBConnectionManager: IB TWS connection and monitoring
- HistoricalDataLoader: Load x weeks of x bar size data
- LiveTickAggregator: Real-time tick-to-OHLC conversion
- TradeExecutor: Direct trade execution with PAPER mode kill switch
- ErlangBridge: Communication with Erlang neural networks
"""

import sys
import struct
import json
import asyncio
import time
import logging
import os
from datetime import datetime, timedelta
from ib_insync import IB, Forex, MarketOrder, LimitOrder, util

# Enable asyncio mode for ib_insync (if available)
try:
    util.useAsyncio()
except AttributeError:
    pass

# Logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Global state
running = True

# Startup options
AUTO_BACKFILL = os.getenv("AUTO_BACKFILL", "0") in ("1", "true", "True")

# Canonical OHLC Schema
# {symbol, t_open, o, h, l, c, vol, source}
# Key ETS by {Symbol, TOpen}, idempotent upsert

# ============================================================================
# 1. IB CONNECTION MANAGER
# ============================================================================

class IBConnectionManager:
    """Manages IB TWS connection with monitoring and auto-reconnection"""

    def __init__(self):
        self.ib = IB()
        self.connection_status = False
        self.subscribed_symbols = set()
        self.last_connect_params = {
            "host": "host.docker.internal",
            "port": 7497,
            "client_id": 101
        }

    async def connect(self, host="host.docker.internal", port=7497, client_id=101):
        """Establish connection to IB TWS with paper-trading enforcement"""
        # Paper-only guard unless ALLOW_LIVE is set
        if port != 7497 and not os.getenv('ALLOW_LIVE'):
            return {"status": "error", "message": "Paper only (port 7497)"}

        try:
            logger.info(f"Connecting to IB {host}:{port} (client_id={client_id})")
            await self.ib.connectAsync(host, port, clientId=client_id, timeout=10)
            self.ib.reqMarketDataType(3)  # 1=real-time, 3=delayed (safe default)
            self.connection_status = True

            # Store for reconnection
            self.last_connect_params.update({
                "host": host, "port": port, "client_id": client_id
            })

            await self.setup_market_data_subscriptions()
            logger.info("Connected to IB successfully")
            return {"status": "connected", "client_id": client_id}

        except Exception as e:
            logger.error(f"Connection failed: {e}")
            return {"status": "error", "message": str(e)}

    async def setup_market_data_subscriptions(self):
        """Subscribe to live market data for configured symbols"""
        symbols = ['EUR.USD', 'GBP.USD', 'USD.JPY']  # From config
        for symbol in symbols:
            try:
                contract = self._parse_symbol(symbol)
                self.ib.reqMktData(contract, '', False, False)
                self.subscribed_symbols.add(symbol)
                logger.info(f"Subscribed to market data for {symbol}")
            except Exception as e:
                logger.error(f"Failed to subscribe to {symbol}: {e}")

    def get_connection_status(self):
        """Get current connection status"""
        return {
            "connected": self.ib.isConnected(),
            "subscribed_symbols": list(self.subscribed_symbols),
            "last_connect": self.last_connect_params
        }

    async def disconnect(self):
        """Disconnect from IB TWS"""
        if self.ib.isConnected():
            self.ib.disconnect()
        self.connection_status = False
        self.subscribed_symbols.clear()

    def _parse_symbol(self, symbol: str):
        """Convert 'EUR.USD' to Forex contract"""
        if '.' in symbol:
            base, quote = symbol.split('.', 1)
            return Forex(base + quote)
        return Forex(symbol)

# ============================================================================
# 2. HISTORICAL DATA LOADER
# ============================================================================

class HistoricalDataLoader:
    """Loads historical data and streams to Erlang bar-by-bar"""

    def __init__(self, ib_connection, bridge):
        self.ib = ib_connection
        self.bridge = bridge  # ErlangBridge instance

    async def load_weeks_of_data(self, symbol, weeks=4, bar_size='1 min'):
        """Load historical data for specified weeks and bar size,
        then send to Erlang one bar at a time via bridge.send_ohlc_bar()"""

        contract = self._parse_symbol(symbol)
        duration = f"{weeks} W"  # e.g., "4 W" for 4 weeks

        try:
            logger.info(f"Loading {weeks} weeks of {bar_size} data for {symbol}")
            bars = await self.ib.reqHistoricalDataAsync(
                contract=contract,
                endDateTime='',           # Current time
                durationStr=duration,
                barSizeSetting=bar_size,
                whatToShow='MIDPOINT',
                useRTH=False,             # Include extended hours
                formatDate=1
            )

            # Convert and stream to Erlang bar-by-bar
            for bar in bars:
                ohlc_bar = {
                    'symbol': symbol,
                    't_open': bar.date.isoformat(),
                    'o': float(bar.open),
                    'h': float(bar.high),
                    'l': float(bar.low),
                    'c': float(bar.close),
                    'vol': int(bar.volume),
                    'source': 'historical'
                }
                self.bridge.send_ohlc_bar(ohlc_bar)

            logger.info(f"Loaded {len(bars)} historical bars for {symbol}")
            return len(bars)

        except Exception as e:
            logger.error(f"Error loading historical data for {symbol}: {e}")
            return 0

    def _parse_symbol(self, symbol: str):
        """Convert 'EUR.USD' to Forex contract"""
        if '.' in symbol:
            base, quote = symbol.split('.', 1)
            return Forex(base + quote)
        return Forex(symbol)

# ============================================================================
# 3. LIVE TICK AGGREGATOR
# ============================================================================

class LiveTickAggregator:
    """Aggregates live ticks into 1-minute OHLC bars"""

    def __init__(self, bridge):
        self.current_bars = {}  # symbol -> current 1-min bar
        self.bar_duration = timedelta(minutes=1)
        self.bridge = bridge

    def process_tick(self, symbol, price, volume, timestamp):
        """Process individual tick and aggregate into 1-minute bars"""
        if not price or price <= 0:
            return

        bar_start = self._get_bar_start_time(timestamp)

        if symbol not in self.current_bars:
            # Start new bar
            self.current_bars[symbol] = {
                'symbol': symbol,
                'start_time': bar_start,
                'o': price,
                'h': price,
                'l': price,
                'c': price,
                'vol': volume or 0,
                'tick_count': 1
            }
        else:
            current_bar = self.current_bars[symbol]

            # Check if we need to complete current bar and start new one
            if timestamp >= current_bar['start_time'] + self.bar_duration:
                # Complete current bar
                completed_bar = self._finalize_bar(symbol, current_bar)

                # Send completed bar to Erlang
                self.bridge.send_ohlc_bar({
                    'symbol': symbol,
                    't_open': completed_bar['start_time'].isoformat(),
                    'o': completed_bar['o'],
                    'h': completed_bar['h'],
                    'l': completed_bar['l'],
                    'c': completed_bar['c'],
                    'vol': completed_bar['vol'],
                    'source': 'live'
                })

                # Start new bar
                self._start_new_bar(symbol, price, volume or 0, bar_start)
            else:
                # Update current bar
                current_bar['h'] = max(current_bar['h'], price)
                current_bar['l'] = min(current_bar['l'], price)
                current_bar['c'] = price
                current_bar['vol'] += volume or 0
                current_bar['tick_count'] += 1

    def _get_bar_start_time(self, timestamp):
        """Get the start time for the 1-minute bar containing this timestamp"""
        if isinstance(timestamp, str):
            timestamp = datetime.fromisoformat(timestamp.replace('Z', '+00:00'))
        elif isinstance(timestamp, (int, float)):
            timestamp = datetime.fromtimestamp(timestamp)

        # Round down to minute boundary
        return timestamp.replace(second=0, microsecond=0)

    def _finalize_bar(self, symbol, bar):
        """Finalize a completed bar"""
        return bar

    def _start_new_bar(self, symbol, price, volume, bar_start):
        """Start a new bar"""
        self.current_bars[symbol] = {
            'symbol': symbol,
            'start_time': bar_start,
            'o': price,
            'h': price,
            'l': price,
            'c': price,
            'vol': volume,
            'tick_count': 1
        }

# ============================================================================
# 4. TRADE EXECUTOR
# ============================================================================

class TradeExecutor:
    """Executes trades directly with IB with PAPER mode kill switch"""

    def __init__(self, ib_connection):
        self.ib = ib_connection
        self.pending_orders = {}
        self.paper_mode_enforced = True  # Default to paper mode

    async def execute_trade(self, symbol, action, quantity, order_type='MKT', limit_price=None):
        """Execute trade directly with IB (respects PAPER kill switch)"""

        # PAPER mode kill switch - env flag + runtime command
        if not os.getenv('ALLOW_LIVE_ORDERS'):
            logger.info("Trade execution blocked: Paper mode enforced (ALLOW_LIVE_ORDERS not set)")
            # Simulate an accepted trade without placing a live order
            fake_id = int(time.time() * 1000) % 100000000
            self.pending_orders[fake_id] = {
                'symbol': symbol,
                'action': action,
                'quantity': quantity,
                'status': 'simulated',
                'timestamp': datetime.now()
            }
            return {
                'order_id': fake_id,
                'status': 'simulated',
                'symbol': symbol,
                'action': action,
                'quantity': quantity
            }

        try:
            contract = self._parse_symbol(symbol)

            if order_type == 'MKT':
                order = MarketOrder(action, quantity)
            elif order_type == 'LMT' and limit_price is not None:
                order = LimitOrder(action, quantity, limit_price)
            else:
                return {'status': 'error', 'message': 'Invalid order type or missing limit price'}

            trade = self.ib.placeOrder(contract, order)

            # Track order
            order_id = trade.order.orderId
            self.pending_orders[order_id] = {
                'symbol': symbol,
                'action': action,
                'quantity': quantity,
                'status': 'pending',
                'timestamp': datetime.now()
            }

            logger.info(f"Order placed: {action} {quantity} {symbol} (ID: {order_id})")

            return {
                'order_id': order_id,
                'status': 'submitted',
                'symbol': symbol,
                'action': action,
                'quantity': quantity
            }

        except Exception as e:
            logger.error(f"Trade execution failed: {e}")
            return {'status': 'error', 'message': str(e)}

    def get_order_status(self, order_id):
        """Get status of a specific order"""
        return self.pending_orders.get(order_id, {'status': 'not_found'})

    def set_paper_mode(self, enabled=True):
        """Enable/disable paper mode kill switch"""
        self.paper_mode_enforced = enabled
        logger.info(f"Paper mode {'enabled' if enabled else 'disabled'}")

    def _parse_symbol(self, symbol: str):
        """Convert 'EUR.USD' to Forex contract"""
        if '.' in symbol:
            base, quote = symbol.split('.', 1)
            return Forex(base + quote)
        return Forex(symbol)

# ============================================================================
# 5. ERLANG BRIDGE
# ============================================================================

class ErlangBridge:
    """Handles communication with Erlang neural networks"""

    def __init__(self):
        self.message_queue = []

    def send_ohlc_bar(self, ohlc_bar):
        """Send processed OHLC bar to Erlang (historical or live)"""
        message = {
            'type': 'ohlc_bar',
            'data': ohlc_bar
        }
        self._send_to_erlang(message)

    def send_trade_confirmation(self, trade_result):
        """Send trade execution result to Erlang"""
        message = {
            'type': 'trade_confirmation',
            'data': trade_result
        }
        self._send_to_erlang(message)

    def send_status_update(self, status_info):
        """Send system status update to Erlang"""
        message = {
            'type': 'status_update',
            'data': status_info
        }
        self._send_to_erlang(message)

    def _send_to_erlang(self, message):
        """Send message to Erlang with proper framing"""
        try:
            b = json.dumps(message, separators=(',', ':')).encode()
            sys.stdout.buffer.write(struct.pack('>I', len(b)) + b)
            sys.stdout.buffer.flush()
        except Exception as e:
            logger.error(f"Error sending to Erlang: {e}")

# ============================================================================
# 6. UTILITY FUNCTIONS
# ============================================================================

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

def send_error(cid, code, message):
    write_msg({"v": 1, "type": "error", "cid": cid, "code": code, "message": message})

def validate_message(msg):
    return isinstance(msg, dict) and all(k in msg for k in ("type", "cid"))

def n(x):
    """NaN-safe helper: NaN→None for JSON"""
    return None if x is None or x != x else x

# ============================================================================
# 7. MAIN SERVICE CLASS
# ============================================================================

class IBService:
    """Main service class coordinating all IB operations"""

    def __init__(self):
        self.connection_manager = IBConnectionManager()
        self.bridge = ErlangBridge()
        self.historical_loader = HistoricalDataLoader(self.connection_manager.ib, self.bridge)
        self.tick_aggregator = LiveTickAggregator(self.bridge)
        self.trade_executor = TradeExecutor(self.connection_manager.ib)
        self.running = True

        # Setup tick forwarding
        self.connection_manager.ib.pendingTickersEvent += self._on_pending_tickers

    async def start_service(self):
        """Start the comprehensive IB service"""
        logger.info("Starting comprehensive IB service")

        # Connect to IB
        result = await self.connection_manager.connect()
        if result["status"] != "connected":
            logger.error(f"Failed to connect: {result}")
            return False

        # Optional: load historical data at startup
        if AUTO_BACKFILL:
            symbols = ['EUR.USD', 'GBP.USD', 'USD.JPY']
            for symbol in symbols:
                await self.historical_loader.load_weeks_of_data(symbol, weeks=4)

        # Start background tasks
        asyncio.create_task(self._heartbeat_task())
        asyncio.create_task(self._connection_monitor_task())

        logger.info("IB service started successfully")
        return True

    async def handle_command(self, cmd):
        """Handle commands from Erlang"""
        cmd_type = cmd.get('type')
        cid = cmd.get('cid')

        try:
            if cmd_type == 'connect':
                await self._handle_connect(cmd, cid)
            elif cmd_type == 'load_historical':
                await self._handle_load_historical(cmd, cid)
            elif cmd_type == 'trade_signal':
                await self._handle_trade_signal(cmd, cid)
            elif cmd_type == 'get_status':
                await self._handle_get_status(cmd, cid)
            elif cmd_type == 'set_paper_mode':
                await self._handle_set_paper_mode(cmd, cid)
            else:
                send_error(cid, "BAD_REQ", f"Unknown command type: {cmd_type}")
        except Exception as e:
            logger.error(f"Error handling command {cmd_type}: {e}")
            send_error(cid, "SERVICE_ERROR", str(e))

    async def _handle_connect(self, cmd, cid):
        """Handle connection request"""
        host = cmd.get('host', 'host.docker.internal')
        port = cmd.get('port', 7497)
        client_id = cmd.get('client_id', 101)

        result = await self.connection_manager.connect(host, port, client_id)
        write_msg({"v": 1, "type": "connect_result", "cid": cid, "result": result})

    async def _handle_load_historical(self, cmd, cid):
        """Handle historical data loading request"""
        symbol = cmd.get('symbol', 'EUR.USD')
        weeks = cmd.get('weeks', 4)
        bar_size = cmd.get('bar_size', '1 min')

        count = await self.historical_loader.load_weeks_of_data(symbol, weeks, bar_size)
        write_msg({
            "v": 1, "type": "historical_loaded", "cid": cid,
            "symbol": symbol, "bars_loaded": count
        })

    async def _handle_trade_signal(self, cmd, cid):
        """Handle trade signal from neural network"""
        symbol = cmd.get('symbol')
        signal = cmd.get('signal')  # 1 (long), -1 (short), 0 (close)
        quantity = cmd.get('quantity', 0.1)

        if signal == 1:
            action = 'BUY'
        elif signal == -1:
            action = 'SELL'
        elif signal == 0:
            # Close position logic would go here
            write_msg({"v": 1, "type": "trade_result", "cid": cid, "action": "close"})
            return
        else:
            send_error(cid, "BAD_SIGNAL", f"Invalid trade signal: {signal}")
            return

        result = await self.trade_executor.execute_trade(symbol, action, quantity)
        self.bridge.send_trade_confirmation(result)
        write_msg({"v": 1, "type": "trade_result", "cid": cid, "result": result})

    async def _handle_get_status(self, cmd, cid):
        """Handle status request"""
        status = {
            "connection": self.connection_manager.get_connection_status(),
            "pending_orders": len(self.trade_executor.pending_orders),
            "current_bars": len(self.tick_aggregator.current_bars),
            "paper_mode": self.trade_executor.paper_mode_enforced
        }
        write_msg({"v": 1, "type": "status", "cid": cid, "status": status})

    async def _handle_set_paper_mode(self, cmd, cid):
        """Handle paper mode setting"""
        enabled = cmd.get('enabled', True)
        self.trade_executor.set_paper_mode(enabled)
        write_msg({"v": 1, "type": "paper_mode_set", "cid": cid, "enabled": enabled})

    def _on_pending_tickers(self, tickers):
        """Handle incoming market ticks"""
        try:
            for ticker in tickers:
                if ticker.bid is None and ticker.ask is None and ticker.last is None:
                    continue

                # Use mid price for aggregation
                price = None
                if ticker.bid is not None and ticker.ask is not None:
                    price = (ticker.bid + ticker.ask) / 2
                elif ticker.last is not None:
                    price = ticker.last

                # Proper FX symbol mapping: SYMBOL.CURRENCY
                symbol_out = self._format_symbol_for_output(ticker.contract)

                if price:
                    self.tick_aggregator.process_tick(
                        symbol_out, price, ticker.volume or 0, datetime.now()
                    )

                # Also send raw tick to Erlang for monitoring
                self.bridge._send_to_erlang({
                    "type": "tick",
                    "symbol": symbol_out,
                    "bid": n(ticker.bid),
                    "ask": n(ticker.ask),
                    "last": n(ticker.last),
                    "volume": n(ticker.volume)
                })
        except Exception as e:
            logger.error(f"Tick processing error: {e}")

    def _format_symbol_for_output(self, contract) -> str:
        """
        Prefer SYMBOL.CURRENCY (e.g., USD.JPY, EUR.USD).
        Falls back to localSymbol split if needed.
        """
        try:
            sym = getattr(contract, "symbol", "") or ""
            cur = getattr(contract, "currency", "") or ""
            if sym and cur:
                return f"{sym}.{cur}"
            loc = getattr(contract, "localSymbol", "") or ""
            if len(loc) == 6:
                return f"{loc[:3]}.{loc[3:]}"
            return loc or sym or "UNKNOWN.FX"
        except Exception:
            return "UNKNOWN.FX"

    async def _heartbeat_task(self):
        """Send periodic heartbeat to Erlang"""
        while self.running:
            try:
                self.bridge._send_to_erlang({
                    "type": "heartbeat",
                    "timestamp": int(time.time() * 1000),
                    "ib_connected": self.connection_manager.ib.isConnected()
                })
            except Exception as e:
                logger.error(f"Heartbeat error: {e}")
            await asyncio.sleep(3)

    async def _connection_monitor_task(self):
        """Monitor IB connection and attempt reconnection"""
        reconnect_attempts = 0
        max_attempts = 5

        while self.running:
            try:
                if not self.connection_manager.ib.isConnected() and reconnect_attempts < max_attempts:
                    logger.info(f"Connection lost, attempting reconnection ({reconnect_attempts + 1}/{max_attempts})")

                    params = self.connection_manager.last_connect_params
                    result = await self.connection_manager.connect(
                        params["host"], params["port"], params["client_id"]
                    )

                    if result["status"] == "connected":
                        logger.info("Reconnection successful")
                        reconnect_attempts = 0
                    else:
                        reconnect_attempts += 1
                        logger.error(f"Reconnection failed: {result}")

                elif reconnect_attempts >= max_attempts:
                    logger.error("Max reconnection attempts reached")
                    self.bridge.send_status_update({"status": "connection_failed"})
                    break

            except Exception as e:
                logger.error(f"Connection monitor error: {e}")

            await asyncio.sleep(5)

    async def shutdown(self):
        """Shutdown the service gracefully"""
        logger.info("Shutting down IB service")
        self.running = False
        await self.connection_manager.disconnect()

# ============================================================================
# 8. MAIN LOOP AND SERVICE COORDINATION
# ============================================================================

# Global service instance
service = None

async def main():
    """Main entry point for comprehensive IB service"""
    global service, running
    logger.info("Comprehensive Python IB service starting up")

    try:
        # Initialize service
        service = IBService()

        # Start service components
        if not await service.start_service():
            logger.error("Failed to start IB service")
            return

        # Main message loop
        while running:
            msg = await read_msg()
            if msg is None:  # EOF
                logger.info("Received EOF, shutting down")
                break
            if not validate_message(msg):
                send_error(None, "BAD_REQ", "Missing required fields")
                continue

            # Single, modern command path
            try:
                await service.handle_command(msg)
            except Exception as e:
                logger.error(f"Error in command loop: {e}")
                send_error(msg.get("cid"), "BRIDGE_IO", str(e))

    except KeyboardInterrupt:
        logger.info("Received interrupt, shutting down")
    finally:
        if service:
            await service.shutdown()
        logger.info("Service shutdown complete")

# ---------------- Entrypoint ----------------

if __name__ == '__main__':
    asyncio.run(main())
