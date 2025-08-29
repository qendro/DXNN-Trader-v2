#!/usr/bin/env python3
"""
Comprehensive Python-Centric IB Service
Handles ALL Interactive Brokers operations: connection, historical data, live streaming,
tick aggregation, and trade execution. Erlang focuses purely on neural networks.

Architecture:
- IBConnectionManager: IB TWS connection and monitoring
- HistoricalDataLoader: Load x weeks of x bar size data
- LiveTickAggregator: Real-time tick-to-OHLC conversion
- TradeExecutor: Direct trade execution with comprehensive kill switches
- ErlangBridge: Communication with Erlang neural networks
"""

import sys
import struct
import json
import asyncio
import time
import logging
import os
import math
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

# ============================================================================
# SIMPLE CONFIG LOADING
# ============================================================================

def load_config():
    """Load configuration from ib_service_config.json or use defaults"""
    default_config = {
        "ib_connection": {
            "host": "host.docker.internal",
            "port": 7497,
            "client_id": 101,
            "timeout": 10
        },
        "trading": {
            "symbols": ["EUR.USD", "GBP.USD", "USD.JPY"],
            "default_quantity": 0.1,
            "max_daily_trades": 50,
            "max_daily_loss": 0.05,
            "max_position_size": 0.2
        },
        "data": {
            "historical_weeks": 1,
            "bar_size": "1 min",
            "preload_on_startup": True
        },
        "monitoring": {
            "heartbeat_interval": 3,
            "connection_monitor_interval": 5,
            "max_reconnect_attempts": 5
        }
    }
    
    try:
        config_path = 'ib_service_config.json'
        if not os.path.exists(config_path):
            config_path = 'priv/ib_service_config.json'
        if os.path.exists(config_path):
            with open(config_path, 'r') as f:
                file_config = json.load(f)
            # Simple merge - file config overrides defaults
            for section, values in file_config.items():
                if section in default_config and isinstance(values, dict):
                    default_config[section].update(values)
                else:
                    default_config[section] = values
            logger.info(f"Loaded configuration from {config_path}")
            logger.info(f"Risk management config: {default_config.get('risk_management', {})}")
        else:
            logger.info(f"Config file {config_path} not found, using default configuration")
    except Exception as e:
        logger.warning(f"Error loading config file: {e}, using defaults")
    
    return default_config

# Load configuration
config = load_config()

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
        """Establish connection to IB TWS"""
        # Safety check for live trading ports
        if port != 7497 and not os.getenv('ALLOW_LIVE'):
            return {"status": "error", "message": "Live trading port requires ALLOW_LIVE=1"}

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
        symbols = config.get("trading", {}).get("symbols", ["EUR.USD"])
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
                    'vol': int(bar.volume) if isinstance(bar.volume, (int, float)) and math.isfinite(bar.volume) else 0,
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
        # Sanitize inputs (handle None/NaN)
        if not isinstance(price, (int, float)) or not math.isfinite(price):
            return
        vol = int(volume) if isinstance(volume, (int, float)) and math.isfinite(volume) and volume > 0 else 0

        bar_start = self._get_bar_start_time(timestamp)

        if symbol not in self.current_bars:
            # Start new bar
            self.current_bars[symbol] = {
                'symbol': symbol,
                'start_time': bar_start,
                'o': float(price),
                'h': float(price),
                'l': float(price),
                'c': float(price),
                'vol': vol,
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
                    'vol': int(completed_bar['vol']) if isinstance(completed_bar['vol'], (int, float)) and math.isfinite(completed_bar['vol']) and completed_bar['vol'] >= 0 else 0,
                    'source': 'live'
                })

                # Start new bar
                self._start_new_bar(symbol, price, vol, bar_start)
            else:
                # Update current bar
                current_bar['h'] = max(current_bar['h'], price)
                current_bar['l'] = min(current_bar['l'], price)
                current_bar['c'] = price
                current_bar['vol'] = (current_bar['vol'] if isinstance(current_bar['vol'], (int, float)) and math.isfinite(current_bar['vol']) else 0) + vol
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
            'o': float(price),
            'h': float(price),
            'l': float(price),
            'c': float(price),
            'vol': int(volume) if isinstance(volume, (int, float)) and math.isfinite(volume) and volume >= 0 else 0,
            'tick_count': 1
        }

# ============================================================================
# 4. TRADE EXECUTOR WITH ENHANCED KILL SWITCH
# ============================================================================

class TradeExecutor:
    """Executes trades with comprehensive kill switch and position management"""

    def __init__(self, ib_connection):
        self.ib = ib_connection
        self.pending_orders = {}
        self.positions = {}  # symbol -> position info
        self.daily_trades = 0
        self.daily_pnl = 0.0
        self.kill_switch_active = False
        
        # Risk limits from config
        trading_config = config.get("trading", {})
        risk_config = config.get("risk_management", {})
        self.max_daily_trades = trading_config.get('max_daily_trades', 50)
        self.max_daily_loss = risk_config.get('daily_loss_limit', 0.05)
        self.max_position_size = risk_config.get('max_position_size', 0.2)

    async def execute_trade(self, symbol, action, quantity, order_type='MKT', limit_price=None):
        """Execute trade with comprehensive safety checks"""
        
        # Kill switch checks
        if self._is_trading_blocked():
            return self._create_blocked_response(symbol, action, quantity)
        
        # Risk checks
        risk_check = self._validate_trade_risk(symbol, action, quantity)
        if risk_check['blocked']:
            return risk_check
        
        # Execute trade directly to IB
        return await self._execute_live_trade(symbol, action, quantity, order_type, limit_price)

    def _is_trading_blocked(self):
        """Check if trading is blocked by kill switches"""
        # Environment kill switch
        if self.kill_switch_active:
            return True
        
        # Daily limits kill switch
        if self.daily_trades >= self.max_daily_trades:
            logger.warning(f"Daily trade limit reached: {self.daily_trades}")
            return True
            
        if self.daily_pnl <= -self.max_daily_loss:
            logger.warning(f"Daily loss limit reached: {self.daily_pnl}")
            return True
            
        return False

    def _validate_trade_risk(self, symbol, action, quantity):
        """Validate trade against risk parameters"""
        # Position size check
        current_position = self.positions.get(symbol, {'quantity': 0})
        new_position_size = abs(current_position['quantity'] + 
                               (quantity if action == 'BUY' else -quantity))
        
        if new_position_size > self.max_position_size:
            return {
                'status': 'error',
                'blocked': True,
                'message': f'Position size limit exceeded: {new_position_size} > {self.max_position_size}'
            }
        
        return {'blocked': False}



    async def _execute_live_trade(self, symbol, action, quantity, order_type, limit_price):
        """Execute trade with IB"""
        try:
            contract = self._parse_symbol(symbol)

            if order_type == 'MKT':
                order = MarketOrder(action, quantity)
            elif order_type == 'LMT' and limit_price is not None:
                order = LimitOrder(action, quantity, limit_price)
            else:
                return {'status': 'error', 'message': 'Invalid order type'}

            trade = self.ib.placeOrder(contract, order)
            order_id = trade.order.orderId

            # Track order
            self.pending_orders[order_id] = {
                'symbol': symbol,
                'action': action,
                'quantity': quantity,
                'status': 'pending',
                'timestamp': datetime.now()
            }

            # Setup order monitoring (fix the updateEvent issue)
            try:
                self._setup_order_monitoring(trade)
            except AttributeError:
                # Handle older ib_insync versions that don't have updateEvent
                logger.info(f"Order monitoring not available for this ib_insync version")
            
            self.daily_trades += 1
            logger.info(f"IB TRADE: {action} {quantity} {symbol} (Order ID: {order_id})")

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

    def _create_blocked_response(self, symbol, action, quantity):
        """Create response for blocked trade"""
        return {
            'status': 'blocked',
            'symbol': symbol,
            'action': action,
            'quantity': quantity,
            'message': 'Trading blocked by kill switch'
        }



    def _setup_order_monitoring(self, trade):
        """Setup monitoring for live orders"""
        def on_order_update(updated_trade):
            try:
                order_id = updated_trade.order.orderId
                status = updated_trade.orderStatus.status
                
                if order_id in self.pending_orders:
                    self.pending_orders[order_id]['status'] = status
                    
                if status in ('Filled', 'PartiallyFilled'):
                    self._handle_fill(updated_trade)
                    
            except Exception as e:
                logger.error(f"Order monitoring error: {e}")
        
        trade.updateEvent += on_order_update

    def _handle_fill(self, trade):
        """Handle order fill"""
        symbol = self._format_symbol_from_contract(trade.contract)
        action = trade.order.action
        filled_qty = trade.orderStatus.filled
        avg_price = trade.orderStatus.avgFillPrice
        
        # Update position
        if symbol not in self.positions:
            self.positions[symbol] = {'quantity': 0, 'avg_price': 0}
        
        if action == 'BUY':
            self.positions[symbol]['quantity'] += filled_qty
        else:
            self.positions[symbol]['quantity'] -= filled_qty
            
        logger.info(f"FILL: {action} {filled_qty} {symbol} @ {avg_price}")

    def activate_kill_switch(self, reason="Manual activation"):
        """Activate emergency kill switch"""
        self.kill_switch_active = True
        logger.warning(f"KILL SWITCH ACTIVATED: {reason}")

    def deactivate_kill_switch(self):
        """Deactivate kill switch"""
        self.kill_switch_active = False
        logger.info("Kill switch deactivated")

    def get_trading_status(self):
        """Get comprehensive trading status"""
        return {
            'kill_switch_active': self.kill_switch_active,
            'daily_trades': self.daily_trades,
            'daily_pnl': self.daily_pnl,
            'pending_orders': len(self.pending_orders),
            'positions': self.positions,
            'ib_connected': self.ib.isConnected() if hasattr(self, 'ib') else False
        }

    def reset_daily_counters(self):
        """Reset daily counters (call at start of new trading day)"""
        self.daily_trades = 0
        self.daily_pnl = 0.0
        logger.info("Daily counters reset")

    def _parse_symbol(self, symbol: str):
        """Convert 'EUR.USD' to Forex contract"""
        if '.' in symbol:
            base, quote = symbol.split('.', 1)
            return Forex(base + quote)
        return Forex(symbol)

    def _format_symbol_from_contract(self, contract):
        """Format symbol from IB contract"""
        try:
            if hasattr(contract, 'symbol') and hasattr(contract, 'currency'):
                return f"{contract.symbol}.{contract.currency}"
            return str(contract.localSymbol or contract.symbol)
        except:
            return "UNKNOWN"

# ============================================================================
# 5. ERLANG BRIDGE - ENHANCED COMMUNICATION
# ============================================================================

class ErlangBridge:
    """Enhanced communication with Erlang neural networks"""

    def __init__(self):
        self.message_count = 0
        self.last_heartbeat = time.time()

    def send_ohlc_bar(self, ohlc_bar):
        """Send OHLC bar using canonical schema"""
        # Ensure canonical format and sanitize NaN/None
        try:
            o = float(ohlc_bar['o'])
            h = float(ohlc_bar['h'])
            l = float(ohlc_bar['l'])
            c = float(ohlc_bar['c'])
            if not all(map(math.isfinite, (o, h, l, c))):
                return  # drop invalid bar
            vol_in = ohlc_bar.get('vol', 0)
            vol = int(vol_in) if isinstance(vol_in, (int, float)) and math.isfinite(vol_in) and vol_in >= 0 else 0
        except Exception:
            return

        canonical_bar = {
            'symbol': ohlc_bar['symbol'],
            't_open': ohlc_bar['t_open'],
            'o': o,
            'h': h,
            'l': l,
            'c': c,
            'vol': vol,
            'source': ohlc_bar['source']
        }
        
        message = {
            'type': 'ohlc_bar',
            'data': canonical_bar,
            'timestamp': int(time.time() * 1000)
        }
        self._send_to_erlang(message)

    def send_trade_confirmation(self, trade_result):
        """Send trade execution result"""
        message = {
            'type': 'trade_confirmation',
            'data': trade_result,
            'timestamp': int(time.time() * 1000)
        }
        self._send_to_erlang(message)

    def send_status_update(self, status_info):
        """Send system status update"""
        message = {
            'type': 'status_update',
            'data': status_info,
            'timestamp': int(time.time() * 1000)
        }
        self._send_to_erlang(message)

    def send_kill_switch_alert(self, reason):
        """Send kill switch activation alert"""
        message = {
            'type': 'kill_switch_alert',
            'data': {'reason': reason, 'active': True},
            'timestamp': int(time.time() * 1000)
        }
        self._send_to_erlang(message)

    def send_risk_violation(self, violation_info):
        """Send risk violation alert"""
        message = {
            'type': 'risk_violation',
            'data': violation_info,
            'timestamp': int(time.time() * 1000)
        }
        self._send_to_erlang(message)

    def send_heartbeat(self, service_status):
        """Send enhanced heartbeat with service status"""
        self.last_heartbeat = time.time()
        # Disabled - no heartbeat messages to Erlang
        # message = {
        #     'type': 'heartbeat',
        #     'data': service_status,
        #     'timestamp': int(time.time() * 1000)
        # }
        # self._send_to_erlang(message)

    def _send_to_erlang(self, message):
        """Send message with proper framing and error handling"""
        try:
            self.message_count += 1
            message['msg_id'] = self.message_count
            
            b = json.dumps(message, separators=(',', ':')).encode()
            sys.stdout.buffer.write(struct.pack('>I', len(b)) + b)
            sys.stdout.buffer.flush()
            
        except Exception as e:
            logger.error(f"Error sending to Erlang: {e}")
            # Could implement retry logic here if needed

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
        should_preload = (config.get("data", {}).get("preload_on_startup", False) or 
                         os.getenv("AUTO_BACKFILL", "0") in ("1", "true", "True"))
        if should_preload:
            symbols = config.get("trading", {}).get("symbols", ["EUR.USD"])
            weeks = config.get("data", {}).get("historical_weeks", 4)
            for symbol in symbols:
                await self.historical_loader.load_weeks_of_data(symbol, weeks=weeks)

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
            logger.info(f"Command received: type={cmd_type}, cid={cid}, payload_keys={list(cmd.keys())}")
        except Exception:
            pass

        try:
            if cmd_type == 'connect':
                await self._handle_connect(cmd, cid)
            elif cmd_type == 'load_historical':
                await self._handle_load_historical(cmd, cid)
            elif cmd_type == 'trade_signal':
                await self._handle_trade_signal(cmd, cid)
            elif cmd_type == 'get_status':
                await self._handle_get_status(cmd, cid)
            elif cmd_type == 'set_trading_mode':
                await self._handle_set_trading_mode(cmd, cid)
            elif cmd_type == 'activate_kill_switch':
                await self._handle_activate_kill_switch(cmd, cid)
            elif cmd_type == 'deactivate_kill_switch':
                await self._handle_deactivate_kill_switch(cmd, cid)
            elif cmd_type == 'get_trading_status':
                await self._handle_get_trading_status(cmd, cid)
            elif cmd_type == 'reset_daily_counters':
                await self._handle_reset_daily_counters(cmd, cid)
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
        logger.info(f"trade_signal received: symbol={symbol}, signal={signal}, qty={quantity}, ALLOW_LIVE_ORDERS={os.getenv('ALLOW_LIVE_ORDERS')}")

        if signal == 1:
            action = 'BUY'
        elif signal == -1:
            action = 'SELL'
        elif signal == 0:
            # Close position - implement position closing logic
            result = await self._close_position(symbol)
            write_msg({"v": 1, "type": "trade_result", "cid": cid, "result": result})
            return
        else:
            send_error(cid, "BAD_SIGNAL", f"Invalid trade signal: {signal}")
            return

        result = await self.trade_executor.execute_trade(symbol, action, quantity)
        logger.info(f"trade_signal result: {result}")
        
        # Send confirmation via bridge and direct response
        self.bridge.send_trade_confirmation(result)
        write_msg({"v": 1, "type": "trade_result", "cid": cid, "result": result})
        
        # Check for risk violations
        if result.get('status') == 'blocked':
            self.bridge.send_risk_violation({
                'type': 'trade_blocked',
                'symbol': symbol,
                'reason': result.get('message')
            })

    async def _close_position(self, symbol):
        """Close existing position for symbol"""
        position = self.trade_executor.positions.get(symbol, {'quantity': 0})
        current_qty = position['quantity']
        
        if current_qty == 0:
            return {'status': 'no_position', 'symbol': symbol}
        
        # Determine close action
        close_action = 'SELL' if current_qty > 0 else 'BUY'
        close_qty = abs(current_qty)
        
        result = await self.trade_executor.execute_trade(symbol, close_action, close_qty)
        result['action_type'] = 'close_position'
        
        return result

    async def _handle_get_status(self, cmd, cid):
        """Handle status request"""
        status = {
            "connection": self.connection_manager.get_connection_status(),
            "pending_orders": len(self.trade_executor.pending_orders),
            "current_bars": len(self.tick_aggregator.current_bars),
            "ib_port": config.get("ib_connection", {}).get("port", 7497)
        }
        write_msg({"v": 1, "type": "status", "cid": cid, "status": status})

    async def _handle_set_trading_mode(self, cmd, cid):
        """Handle trading mode setting"""
        # All trades go directly to IB - trading mode is determined by IB connection port
        current_port = config.get("ib_connection", {}).get("port", 7497)
        mode = "Paper Trading" if current_port == 7497 else "Live Trading"
        write_msg({
            "v": 1, "type": "trading_mode_status", "cid": cid, 
            "port": current_port,
            "mode": mode,
            "message": f"All trades sent directly to IB. Current mode: {mode} (Port {current_port})"
        })

    async def _handle_activate_kill_switch(self, cmd, cid):
        """Handle kill switch activation"""
        reason = cmd.get('reason', 'Manual activation via command')
        self.trade_executor.activate_kill_switch(reason)
        self.bridge.send_kill_switch_alert(reason)
        write_msg({"v": 1, "type": "kill_switch_activated", "cid": cid, "reason": reason})

    async def _handle_deactivate_kill_switch(self, cmd, cid):
        """Handle kill switch deactivation"""
        self.trade_executor.deactivate_kill_switch()
        write_msg({"v": 1, "type": "kill_switch_deactivated", "cid": cid})

    async def _handle_get_trading_status(self, cmd, cid):
        """Handle trading status request"""
        status = self.trade_executor.get_trading_status()
        write_msg({"v": 1, "type": "trading_status", "cid": cid, "status": status})

    async def _handle_reset_daily_counters(self, cmd, cid):
        """Handle daily counter reset"""
        self.trade_executor.reset_daily_counters()
        write_msg({"v": 1, "type": "daily_counters_reset", "cid": cid})

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

                # Sanitize price and volume
                if isinstance(price, (int, float)) and math.isfinite(price):
                    vol = ticker.volume
                    vol = vol if isinstance(vol, (int, float)) and math.isfinite(vol) and vol > 0 else 0
                    self.tick_aggregator.process_tick(symbol_out, price, vol, datetime.now())

                # Also send raw tick to Erlang for monitoring
                '''   - If you want to send tik data to Erlang
                self.bridge._send_to_erlang({
                    "type": "tick",
                    "symbol": symbol_out,
                    "bid": n(ticker.bid),
                    "ask": n(ticker.ask),
                    "last": n(ticker.last),
                    "volume": n(ticker.volume)
                
                })
                '''
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
        """Send enhanced heartbeat with service status"""
        while self.running:
            try:
                service_status = {
                    "ib_connected": self.connection_manager.ib.isConnected(),
                    "subscribed_symbols": len(self.connection_manager.subscribed_symbols),
                    "pending_orders": len(self.trade_executor.pending_orders),
                    "active_positions": len(self.trade_executor.positions),
                    "kill_switch_active": self.trade_executor.kill_switch_active,
                    "daily_trades": self.trade_executor.daily_trades,
                    "ib_connected": self.connection_manager.ib.isConnected()
                }
                # Disabled - no heartbeat messages to Erlang
                # self.bridge.send_heartbeat(service_status)
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
# 8. MAIN LOOP - CLEAN AND SIMPLE
# ============================================================================

service = None

async def main():
    """Main entry point - clean and simple"""
    global service, running
    logger.info("Python IB service starting")

    try:
        service = IBService()
        
        if not await service.start_service():
            logger.error("Failed to start service")
            return

        # Simple message loop
        while running:
            msg = await read_msg()
            if msg is None:
                logger.info("EOF received, shutting down")
                break
            if not validate_message(msg):
                send_error(None, "BAD_REQ", "Invalid message format")
                continue

            await service.handle_command(msg)

    except KeyboardInterrupt:
        logger.info("Interrupt received, shutting down")
    finally:
        if service:
            await service.shutdown()
        logger.info("Service shutdown complete")

# ---------------- Entrypoint ----------------

if __name__ == '__main__':
    asyncio.run(main())
