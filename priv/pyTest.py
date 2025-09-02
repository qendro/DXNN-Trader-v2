# pyTest.py
# docker run -it --rm --network host -v ${PWD}:/app -w /app erlang-dev bash
# python3
# from ib_insync import *
# ib = IB()
# ib.connect('127.0.0.1', 7497, clientId=1)
# ib.reqMarketDataType(3)
# Forex('EURUSD')
# bars = ib.reqHistoricalData(Forex('EURUSD'), endDateTime='', durationStr='1 D', barSizeSetting='1 min', whatToShow='MIDPOINT', useRTH=False, keepUpToDate=False)
# for b in bars[-10:]:
#    print(b.date, b.open, b.high, b.low, b.close, b.volume)

# for b in bars[-10:]:
#    print(b.date, b.open, b.high, b.low, b.close, b.volume)

# import priv.pyTest as m
# m.connect()
# print(m.fetch_last_5min_1min())
# m.poll_every_minute()
# m.fetch_last_1day_1min() 
# m.fetch_fx('EURUSD', '1 D', '1 min', 'MIDPOINT')
# m.fetch_fx_to_txt('EURUSD', '1 D', '1 min', 'MIDPOINT', 'fx_data.txt')
# m.fetch_fx_to_txt('EURUSD', '4 W', '1 min', 'MIDPOINT', 'fx_data4.txt')
# m.fetch_fx_to_txt('EURUSD', '4 W', '1 min', 'MIDPOINT', 'fx_data5.txt')
# m.fetch_fx_to_txt_chunked('EURUSD', '1 Y', '1 min', 'MIDPOINT', 'fx_data_chunked.txt')
# m.fetch_fx_to_txt_chunked('EURUSD', '1 D', '1 min', 'MIDPOINT', 'fx_data_chunked_1d.txt')
# m.fetch_fx_to_txt_1w_1min(ib, symbol='EURUSD', total_duration='3 Y', whatToShow='MIDPOINT', filename='eurusd_1min_3y.txt')
# m.fetch_fx_to_txt_1w_1min('EURUSD', '3 Y', 'MIDPOINT', 'eurusd_1min_3y.txt')
# m.fetch_fut_to_txt_1w_1min(symbol='QM', expiry='202512', total_duration='5 Y')
# m.fetch_fut_to_txt_1w_1min('MCL', 'NYMEX', '1 Y', 'TRADES', 'mcl_1min_1y.txt')
# m.fetch_fut_expiries_to_csv('MCL', 'NYMEX', '5 Y', 'mcl_expiries.csv')
# m.save_past_expiries_csv('MCL', 'NYMEX', 5, 'mcl_expiries_past_5y.csv')
# m.run_eurusd_live_1min_to_txt('eurusd_1min_live.txt')
# m.stream_eurusd_ticks_live()

# m.fetch_fx_to_txt_chunked('EURUSD', '10 D', '1 sec', filename='eurusd_1s_10d.txt')


from datetime import datetime, timedelta, timezone
import os
from ib_insync import IB, Forex, Future, ContFuture

ib = IB()
ib.RequestTimeout = 1000  # generous HMDS timeout

def connect(port=7497, clientId=1):
    print(f"Connecting to host.docker.internal:{port} (clientId={clientId})")
    ib.connect('host.docker.internal', port, clientId)
    ib.reqMarketDataType(3)  # delayed ok if you lack real-time
    print("Connected.")

def fetch_last_5min_1min():
    ib.qualifyContracts(Forex('EURUSD'))
    bars = ib.reqHistoricalData(
        Forex('EURUSD'),
        endDateTime=datetime.now(timezone.utc),
        durationStr='300 S',
        barSizeSetting='1 min',
        whatToShow='MIDPOINT',
        useRTH=False,
        keepUpToDate=False
    )
    return bars

def poll_every_minute():
    try:
        while True:
            ib.qualifyContracts(Forex('EURUSD'))
            bars = ib.reqHistoricalData(
                Forex('EURUSD'),
                endDateTime=datetime.now(timezone.utc),
                durationStr='5 M',
                barSizeSetting='1 min',
                whatToShow='MIDPOINT',
                useRTH=False,
                keepUpToDate=False
            )
            if bars:
                b = bars[-1]
                dt_utc = b.date.astimezone(timezone.utc) if b.date.tzinfo else b.date.replace(tzinfo=timezone.utc)
                print(f"{dt_utc} O={b.open} H={b.high} L={b.low} C={b.close} V={b.volume}")
            else:
                print("No bars returned.")
            ib.sleep(60)
    except KeyboardInterrupt:
        print("Stopped.")

def fetch_last_1day_1min():
    ib.qualifyContracts(Forex('EURUSD'))
    bars = ib.reqHistoricalData(
        Forex('EURUSD'),
        endDateTime=datetime.now(timezone.utc),
        durationStr='1 D',
        barSizeSetting='1 min',
        whatToShow='MIDPOINT',
        useRTH=False,
        keepUpToDate=False
    )
    if bars:
        print("=== EURUSD 1-minute bars for last 1 day [MIDPOINT] ===")
        for b in bars:
            dt_utc = b.date.astimezone(timezone.utc) if b.date.tzinfo else b.date.replace(tzinfo=timezone.utc)
            print(f"{dt_utc} O={b.open} H={b.high} L={b.low} C={b.close} V={b.volume}")
    else:
        print("No bars returned.")

def fetch_fx(symbol='EURUSD', duration='1 D', bar_size='1 min', feed='MIDPOINT'):
    # 💱 Symbols: "EURUSD","GBPUSD","USDJPY","AUDUSD","USDCAD","NZDUSD","USDCHF"
    # ⏳ Durations: "3600 S","1 D","5 D","1 W","1 M","3 M","1 Y"
    # 📊 Bar sizes: "1 sec","5 secs","15 secs","30 secs","1 min","2 mins","5 mins","15 mins","30 mins","1 hour","1 day"
    ib.qualifyContracts(Forex(symbol))
    bars = ib.reqHistoricalData(
        Forex(symbol),
        endDateTime=datetime.now(timezone.utc),
        durationStr=duration,
        barSizeSetting=bar_size,
        whatToShow=feed,
        useRTH=False,
        keepUpToDate=False
    )
    if bars:
        print(f"=== {symbol} {bar_size} bars for last {duration} [{feed}] ===")
        for b in bars:
            dt_utc = b.date.astimezone(timezone.utc) if b.date.tzinfo else b.date.replace(tzinfo=timezone.utc)
            print(f"{dt_utc} O={b.open} H={b.high} L={b.low} C={b.close} V={b.volume}")
    else:
        print(f"No bars returned for {symbol} [{feed}].")

def fetch_fx_to_txt(symbol='EURUSD', duration='1 D', bar_size='1 min', feed='MIDPOINT', filename='fx_data.txt'):
    ib.qualifyContracts(Forex(symbol))
    bars = ib.reqHistoricalData(
        Forex(symbol),
        endDateTime=datetime.now(timezone.utc),
        durationStr=duration,
        barSizeSetting=bar_size,
        whatToShow=feed,
        useRTH=False,
        keepUpToDate=False
    )
    if not bars:
        print(f"No bars returned for {symbol} [{feed}]."); return
    with open(filename, 'w') as f:
        f.write(f"{symbol} {bar_size} bars for last {duration} [{feed}]\n")
        f.write("Date,Open,High,Low,Close,Volume\n")
        for b in bars:
            dt_utc = b.date.astimezone(timezone.utc) if b.date.tzinfo else b.date.replace(tzinfo=timezone.utc)
            f.write(f"{dt_utc.strftime('%Y-%m-%d %H:%M:%S')},{b.open},{b.high},{b.low},{b.close},{b.volume}\n")
    print(f"Saved {len(bars)} bars for {symbol} to {filename}")

def fetch_fx_to_txt_1w_1min(symbol='EURUSD',
                            total_duration='1 Y',   # '1 Y' or '3 Y'
                            whatToShow='MIDPOINT',
                            filename='fx_1min_1w.txt'):
    """
    Pull 1-minute FX bars in 1-week chunks, <=2 requests/min.
    Uses global `ib` connection. Simple and robust.
    """
    # --- parse and validate duration ---
    parts = total_duration.strip().split()
    if len(parts) != 2 or parts[1] != 'Y':
        raise ValueError("total_duration must be like '1 Y' or '3 Y'")
    years = int(parts[0])
    if years not in (1, 3):
        raise ValueError("Only '1 Y' or '3 Y' supported in this simple helper")

    cutoff = datetime.now(timezone.utc) - timedelta(days=365 * years)

    # --- prepare contract ---
    contract = Forex(symbol)
    ib.qualifyContracts(contract)

    end_dt = datetime.now(timezone.utc)  # explicit UTC now on first call
    seen = set()           # dedupe by datetime
    rows = []

    def req_once(endDateTime):
        return ib.reqHistoricalData(
            contract=contract,
            endDateTime=endDateTime,   # aware UTC datetime
            durationStr='1 W',
            barSizeSetting='1 min',
            whatToShow=whatToShow,
            useRTH=False,
            keepUpToDate=False
        )

    from collections import deque
    recent = deque(maxlen=2)

    def throttle():
        if len(recent) < 2:
            return
        now = datetime.now(timezone.utc)
        delta = (now - recent[0]).total_seconds()
        if delta < 60:
            ib.sleep(60 - delta + 0.05)

    while True:
        try:
            throttle()
            sent_at = datetime.now(timezone.utc)
            bars = req_once(end_dt)
            recent.append(sent_at)
        except Exception as e:
            print(f"Request failed, stopping: {e}")
            break

        if not bars:
            print("No more bars returned, stopping.")
            break

        oldest_dt = bars[0].date
        # keep only >= cutoff and dedupe
        for b in bars:
            if b.date >= cutoff and b.date not in seen:
                seen.add(b.date)
                rows.append(b)

        # stop if we reached or passed cutoff
        if oldest_dt <= cutoff:
            break

        # step back one second from oldest bar to avoid overlap gaps
        end_dt = oldest_dt - timedelta(seconds=1)

    # write file
    rows.sort(key=lambda x: x.date)
    if not rows:
        print(f"No bars saved for {symbol}.")
        return

    with open(filename, 'w', encoding='utf-8') as f:
        f.write(f"{symbol} 1-min bars (1 W chunks) over last {total_duration} [{whatToShow}]\n")
        f.write("Date,Open,High,Low,Close,Volume\n")
        for b in rows:
            dt_utc = b.date.astimezone(timezone.utc) if b.date.tzinfo else b.date.replace(tzinfo=timezone.utc)
            f.write(f"{dt_utc.strftime('%Y-%m-%d %H:%M:%S')},{b.open},{b.high},{b.low},{b.close},{b.volume}\n")

    print(f"Saved {len(rows)} bars for {symbol} to {filename}")

def fetch_fx_to_txt_chunked(symbol: str,
                            duration: str,
                            bar_size: str,
                            whatToShow: str = 'MIDPOINT',
                            filename: str = 'fx_chunked.txt') -> None:
    """
    Fetch FX bars and append to `filename`, chunking by bar size with fallbacks.
    - bar_size: '1 sec' → chunks '2 D' (fallback '1 D'); '1 min' → '1 W' (fallback '3 D'); '1 day' → '1 Y' (fallback '6 M')
    - duration: 'N U' where U ∈ {D,W,M,Y}; rounds up chunk math implicitly by going to now.
    - Resumes from last timestamp in file if it exists; writes incrementally.
    - Pacing: max 2 requests per 60 seconds (sliding window).
    Output rows: UTC 'YYYY-MM-DD HH:MM:SS',Open,High,Low,Close,Volume
    """
    from collections import deque

    # Validate + normalize bar size to IB tokens
    user_bar = bar_size.strip().lower()
    if user_bar == '1 sec':
        ib_bar = '1 secs'   # IB expects plural 'secs'
    elif user_bar == '1 min':
        ib_bar = '1 min'
    elif user_bar == '1 day':
        ib_bar = '1 day'
    else:
        raise ValueError("bar_size must be one of: '1 sec', '1 min', '1 day'")

    # Parse duration like 'N U'
    try:
        n_str, unit = duration.strip().split()
        n = int(n_str)
        unit = unit.upper()
        if unit not in ('D', 'W', 'M', 'Y'):
            raise ValueError
    except Exception:
        raise ValueError("duration must be like '10 D', '3 W', '2 M', or '1 Y'")

    # Convert requested duration to a timedelta (simple approximations)
    days_per_unit = {'D': 1, 'W': 7, 'M': 30, 'Y': 365}
    total_days = n * days_per_unit[unit]

    # Chunk mapping and fallback per bar size
    if bar_size == '1 sec':
        primary_chunk, fallback_chunk = ('2 D', '1 D')
        primary_td, fallback_td = (timedelta(days=2), timedelta(days=1))
    elif bar_size == '1 min':
        primary_chunk, fallback_chunk = ('1 W', '3 D')
        primary_td, fallback_td = (timedelta(days=7), timedelta(days=3))
    else:  # '1 day'
        primary_chunk, fallback_chunk = ('1 Y', '6 M')
        primary_td, fallback_td = (timedelta(days=365), timedelta(days=180))

    # Prepare contract
    contract = Forex(symbol)
    ib.qualifyContracts(contract)

    now_utc = datetime.now(timezone.utc)

    # Determine resume point from file, if any
    last_written_dt = None
    new_file = not os.path.exists(filename)
    if not new_file:
        try:
            with open(filename, 'r', encoding='utf-8') as rf:
                last = None
                for line in rf:
                    line = line.strip()
                    if line and line[0].isdigit() and ',' in line:
                        last = line
                if last:
                    ts = last.split(',')[0].strip()
                    try:
                        last_written_dt = datetime.strptime(ts, '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc)
                    except ValueError:
                        last_written_dt = None
        except FileNotFoundError:
            new_file = True

    requested_start = now_utc - timedelta(days=total_days)
    # Resume from last+1s if file exists; else from requested start
    start_from = (last_written_dt + timedelta(seconds=1)) if last_written_dt else requested_start
    if start_from > now_utc:
        print('Up-to-date: nothing to fetch.')
        return

    # Prepare output file (append mode, add header if new)
    f = open(filename, 'a', encoding='utf-8')
    try:
        if new_file:
            f.write('Date,Open,High,Low,Close,Volume\n')
            f.flush()

        # Pacing: max 2 requests per 60 seconds
        recent = deque(maxlen=2)

        def throttle():
            if len(recent) < 2:
                return
            now = datetime.now(timezone.utc)
            delta = (now - recent[0]).total_seconds()
            if delta < 60:
                ib.sleep(60 - delta + 0.05)

        chunk_str = primary_chunk
        chunk_td = primary_td

        def request(end_dt):
            return ib.reqHistoricalData(
                contract=contract,
                endDateTime=end_dt,
                durationStr=chunk_str,
                barSizeSetting=ib_bar,
                whatToShow=whatToShow,
                useRTH=False,
                keepUpToDate=False
            )

        cursor_end = min(start_from + chunk_td, now_utc)
        while start_from <= now_utc:
            throttle()
            try:
                bars = request(cursor_end)
                recent.append(datetime.now(timezone.utc))
            except Exception as e:
                # Apply single-step fallback per mapping
                if chunk_str == primary_chunk:
                    print(f"Chunk '{chunk_str}' failed ({e}); falling back to '{fallback_chunk}'")
                    chunk_str = fallback_chunk
                    chunk_td = fallback_td
                    cursor_end = min(start_from + chunk_td, now_utc)
                    throttle()
                    try:
                        bars = request(cursor_end)
                        recent.append(datetime.now(timezone.utc))
                    except Exception as e2:
                        print(f"Fallback '{chunk_str}' failed: {e2}")
                        break
                else:
                    print(f"Request failed on '{chunk_str}': {e}")
                    break

            if not bars:
                start_from = cursor_end
                if start_from >= now_utc:
                    break
                cursor_end = min(start_from + chunk_td, now_utc)
                continue

            wrote_any = False
            for b in bars:
                dt = b.date
                dt = dt.astimezone(timezone.utc) if dt.tzinfo else dt.replace(tzinfo=timezone.utc)
                if dt < start_from:
                    continue
                if last_written_dt and dt <= last_written_dt:
                    continue
                f.write(f"{dt.strftime('%Y-%m-%d %H:%M:%S')},{b.open},{b.high},{b.low},{b.close},{b.volume}\n")
                wrote_any = True
                last_written_dt = dt

            if wrote_any:
                f.flush()

            start_from = cursor_end
            if start_from >= now_utc:
                break
            cursor_end = min(start_from + chunk_td, now_utc)

        print(f"Done. Wrote up to {last_written_dt.strftime('%Y-%m-%d %H:%M:%S') if last_written_dt else 'start'} UTC to {filename}.")
    finally:
        f.close()

def fetch_fut_expiries_to_csv(symbol='MCL',
                              exchange='NYMEX',
                              total_duration='5 Y',
                              filename='fut_expiries.csv'):
    """
    Fetch all futures contracts for `symbol` listed on `exchange` within
    the last `total_duration` and save contract months + expiry dates to a CSV.
    Uses global `ib`.
    """
    # --- parse/validate duration ---
    n, unit = total_duration.strip().split()
    if unit != 'Y':
        raise ValueError("total_duration must be like '5 Y'")
    years = int(n)
    if not (1 <= years <= 10):
        raise ValueError("Supported years: 1..10")

    cutoff = datetime.now(timezone.utc) - timedelta(days=365 * years)

    # --- fetch all contract details ---
    cds = ib.reqContractDetails(Future(symbol=symbol, exchange=exchange))
    rows = []

    def parse_ltd(s: str) -> datetime | None:
        s = s.strip()
        for fmt in ("%Y%m%d %H:%M:%S", "%Y%m%d", "%Y%m"):
            try:
                return datetime.strptime(s, fmt).replace(tzinfo=timezone.utc)
            except ValueError:
                continue
        return None

    for cd in cds:
        s = cd.contract.lastTradeDateOrContractMonth
        if not s:
            continue
        dt = parse_ltd(s)
        if not dt:
            continue
        if dt >= cutoff:
            rows.append((cd.contract.localSymbol, s, dt.strftime('%Y-%m-%d %H:%M:%S')))

    # --- sort by expiry date ---
    rows.sort(key=lambda x: x[2])

    if not rows:
        print(f"No contracts found for {symbol} in last {total_duration}.")
        return

    # --- write CSV ---
    with open(filename, 'w', encoding='utf-8') as f:
        f.write("LocalSymbol,ExpiryStr,ExpiryDateUTC\n")
        for localSymbol, expiry_str, expiry_dt in rows:
            f.write(f"{localSymbol},{expiry_str},{expiry_dt}\n")

    print(f"Saved {len(rows)} contracts for {symbol} to {filename}")

def save_past_expiries_csv(symbol='MCL', exchange='NYMEX',
                           years_back=5, filename='mcl_expiries_past_5y.csv'):
    """
    Save all *past* futures expiries (<= now) within the last `years_back` years.
    Columns: LocalSymbol,ContractMonth,ExpiryStr,ExpiryDateUTC,ConId
    """
    now = datetime.now(timezone.utc)
    cutoff = now - timedelta(days=365 * years_back)

    # includeExpired=True is the key change
    cds = ib.reqContractDetails(Future(symbol=symbol, exchange=exchange, includeExpired=True))

    def parse_dt(s: str):
        s = s.strip()
        for fmt in ("%Y%m%d %H:%M:%S", "%Y%m%d", "%Y%m"):
            try:
                return datetime.strptime(s, fmt).replace(tzinfo=timezone.utc)
            except ValueError:
                pass
        return None

    rows = []
    for cd in cds:
        s = cd.contract.lastTradeDateOrContractMonth
        if not s:
            continue
        dt = parse_dt(s)
        if not dt:
            continue
        if cutoff <= dt <= now:
            rows.append((
                cd.contract.localSymbol or '',
                s[:6],
                s,
                dt.strftime('%Y-%m-%d %H:%M:%S'),
                cd.contract.conId
            ))

    # newest → oldest
    rows.sort(key=lambda r: r[3], reverse=True)

    if not rows:
        print(f"No past expiries found for {symbol} in last {years_back} years.")
        return

    with open(filename, 'w', encoding='utf-8') as f:
        f.write("LocalSymbol,ContractMonth,ExpiryStr,ExpiryDateUTC,ConId\n")
        for r in rows:
            f.write("{},{},{},{},{}\n".format(*r))

    print(f"Saved {len(rows)} past expiries for {symbol} to {filename}")

def fetch_fut_to_txt_1w_1min(symbol='QM',
                             exchange='NYMEX',
                             total_duration='1 Y',      # '1 Y'..'5 Y' ok
                             whatToShow='TRADES',
                             filename='qm_1min_1w.txt'):
    """
    Pull 1-minute CONTINUOUS futures bars (front month) in 1-week chunks,
    pacing <= 2 requests/min, with a no-progress guard.
    Uses global `ib`.
    """
    # --- parse/validate duration ---
    n, unit = total_duration.strip().split()
    if unit != 'Y':
        raise ValueError("total_duration must be like '1 Y'")
    years = int(n)
    if not (1 <= years <= 5):
        raise ValueError("Supported years: 1..5 (simple helper)")

    cutoff = datetime.now(timezone.utc) - timedelta(days=365 * years)

    # --- continuous contract ---
    contract = ContFuture(symbol=symbol, exchange=exchange)
    ib.qualifyContracts(contract)

    end_dt = datetime.now(timezone.utc)  # explicit UTC now (first call)
    prev_oldest = None
    did_jump_back = False

    seen = set()                # dedupe by datetime
    rows = []
    fetched = 0

    def req_once(endDateTime):
        return ib.reqHistoricalData(
            contract=contract,
            endDateTime=endDateTime,     # aware UTC datetime
            durationStr='1 W',
            barSizeSetting='1 min',
            whatToShow=whatToShow,
            useRTH=False,
            keepUpToDate=False
        )

    from collections import deque
    recent = deque(maxlen=2)

    def throttle():
        if len(recent) < 2:
            return
        now = datetime.now(timezone.utc)
        delta = (now - recent[0]).total_seconds()
        if delta < 60:
            ib.sleep(60 - delta + 0.05)

    while True:
        throttle()
        sent_at = datetime.now(timezone.utc)
        bars = req_once(end_dt)
        recent.append(sent_at)
        fetched += 1
        print(f"success [{fetched}]: end_dt='{end_dt}'")

        if not bars:
            print("No more bars returned, stopping.")
            break

        oldest_dt = bars[0].date

        # keep & dedupe
        for b in bars:
            if b.date >= cutoff and b.date not in seen:
                seen.add(b.date)
                rows.append(b)

        # reached cutoff?
        if oldest_dt <= cutoff:
            break

        # no-progress guard: if oldest didn't move strictly older, force a jump, else stop
        if prev_oldest is not None and oldest_dt >= prev_oldest:
            if not did_jump_back:
                forced = (prev_oldest - timedelta(days=8))
                print(f"No progress (oldest {oldest_dt}); forcing jump to {forced}")
                end_dt = forced
                did_jump_back = True
                continue
            else:
                print("No progress after forced jump; stopping to avoid a loop.")
                break

        prev_oldest = oldest_dt
        did_jump_back = False

        # normal step: 1s before oldest bar
        end_dt = oldest_dt - timedelta(seconds=1)

    rows.sort(key=lambda x: x.date)
    if not rows:
        print(f"No bars saved for {symbol} (continuous).")
        return

    with open(filename, 'w', encoding='utf-8') as f:
        f.write(f"{symbol} (continuous) 1-min bars, 1W chunks, last {total_duration} [{whatToShow}]\n")
        f.write("Date,Open,High,Low,Close,Volume\n")
        for b in rows:
            dt_utc = b.date.astimezone(timezone.utc) if b.date.tzinfo else b.date.replace(tzinfo=timezone.utc)
            f.write(f"{dt_utc.strftime('%Y-%m-%d %H:%M:%S')},{b.open},{b.high},{b.low},{b.close},{b.volume}\n")

    print(f"Saved {len(rows)} bars for {symbol} (continuous) to {filename}")

def run_eurusd_live_1min_to_txt(filename='eurusd_1min_live.txt'):
    """
    Subscribe to live EURUSD ticks, aggregate to 1-minute bars (mid = (bid+ask)/2),
    and append a snapshot line to `filename` every second.
    Writes a final snapshot (Completed=1) at the end of each minute.
    Ctrl+C to stop.
    """
    contract = Forex('EURUSD')
    ib.qualifyContracts(contract)

    # Subscribe to streaming market data (no snapshot, no regulatory snapshot)
    ticker = ib.reqMktData(contract, '', False, False)

    # Prepare file (header if new)
    new_file = not os.path.exists(filename)
    f = open(filename, 'a', encoding='utf-8')
    if new_file:
        f.write("MinuteUTC,Open,High,Low,Close,NumTicks,Completed\n")

    # Aggregator state for the current minute
    cur_minute = None  # minute timestamp (YYYY-mm-dd HH:MM:00+00:00)
    o = h = l = c = None
    num_ticks = 0

    def minute_floor(dt: datetime) -> datetime:
        # Normalize to UTC and floor to minute
        if dt.tzinfo is None:
            dt = dt.replace(tzinfo=timezone.utc)
        else:
            dt = dt.astimezone(timezone.utc)
        return dt.replace(second=0, microsecond=0)

    print("Streaming EURUSD… (Ctrl+C to stop)")
    try:
        last_seen = None  # to detect changes
        while True:
            ib.sleep(1)  # tick every second

            # Use the freshest snapshot we have
            # For FX, there are no 'trades'; take mid as (bid+ask)/2 when both present.
            bid = ticker.bid
            ask = ticker.ask
            t = ticker.time or datetime.now(timezone.utc)  # fallback if no timestamp yet

            if bid is None or ask is None:
                # no usable tick yet; still emit a heartbeat if we’re already in a minute
                if cur_minute is not None:
                    # append a snapshot with last known c if any
                    snap_close = c if c is not None else ''
                    f.write(f"{cur_minute.strftime('%Y-%m-%d %H:%M:%S')},{o or ''},{h or ''},{l or ''},{snap_close},{num_ticks},0\n")
                    f.flush()
                continue

            mid = (bid + ask) / 2.0
            minute_ts = minute_floor(t)

            # if first bar or minute rollover
            if cur_minute is None:
                cur_minute = minute_ts
                o = h = l = c = mid
                num_ticks = 1
            elif minute_ts != cur_minute:
                # finalize the previous minute: write one last snapshot with Completed=1
                f.write(f"{cur_minute.strftime('%Y-%m-%d %H:%M:%S')},{o},{h},{l},{c},{num_ticks},1\n")
                f.flush()

                # start new minute
                cur_minute = minute_ts
                o = h = l = c = mid
                num_ticks = 1
            else:
                # update current minute bar
                c = mid
                if h is None or mid > h: h = mid
                if l is None or mid < l: l = mid
                num_ticks += 1

            # Append a snapshot EVERY SECOND (Completed=0 while minute is open)
            f.write(f"{cur_minute.strftime('%Y-%m-%d %H:%M:%S')},{o},{h},{l},{c},{num_ticks},0\n")
            f.flush()

    except KeyboardInterrupt:
        # On exit, finalize the open bar if we have one
        if cur_minute is not None and o is not None:
            f.write(f"{cur_minute.strftime('%Y-%m-%d %H:%M:%S')},{o},{h},{l},{c},{num_ticks},1\n")
            f.flush()
        print("\nStopped.")
    finally:
        f.close()
        # keep subscription alive if you want; otherwise:
        # ib.cancelMktData(ticker)

def run_eurusd_1min_ohlc_to_txt(filename='eurusd_1min.txt'):
    """
    Stream live EURUSD quotes, aggregate to 1-minute OHLC using mid=(bid+ask)/2,
    and append ONLY the finalized 1-minute bars to `filename` in format:
    YYYY.MM.DD,H:MM,OPEN,HIGH,LOW,CLOSE,0
    """
    contract = Forex('EURUSD')
    ib.qualifyContracts(contract)
    ticker = ib.reqMktData(contract, '', False, False)

    # prepare file (no header; lines only)
    f = open(filename, 'a', encoding='utf-8')

    def minute_floor_utc(dt: datetime) -> datetime:
        # ensure timezone-aware UTC, then floor to minute
        if dt.tzinfo is None:
            dt = dt.replace(tzinfo=timezone.utc)
        else:
            dt = dt.astimezone(timezone.utc)
        return dt.replace(second=0, microsecond=0)

    cur_minute = None
    o = h = l = c = None
    have_price = False      # have we seen any mid this minute?
    last_mid = None         # carry forward if a minute has no updates

    print("Streaming EURUSD 1-min bars… (Ctrl+C to stop)")
    try:
        while True:
            ib.sleep(1)  # sample once per second from the live stream

            # read latest quotes; FX has bid/ask, not prints
            bid, ask = ticker.bid, ticker.ask
            t = ticker.time or datetime.now(timezone.utc)
            minute_ts = minute_floor_utc(t)

            # if we have both sides, compute mid; else reuse last_mid
            mid = None
            if bid is not None and ask is not None:
                mid = (bid + ask) / 2.0
                last_mid = mid

            if cur_minute is None:
                # initialize on first loop; if no mid yet, wait until we have one
                cur_minute = minute_ts
                if last_mid is not None:
                    o = h = l = c = last_mid
                    have_price = True
                else:
                    have_price = False
                continue

            if minute_ts != cur_minute:
                # minute rolled → finalize prior bar
                if not have_price and last_mid is not None:
                    # no ticks this minute: create a flat bar from last_mid
                    o = h = l = c = last_mid
                    have_price = True

                if have_price:
                    # write finalized bar in requested format (UTC clock)
                    date_str = cur_minute.strftime('%Y.%m.%d')
                    time_str = f"{cur_minute.hour}:{cur_minute.minute:02d}"  # no leading 0 on hour
                    f.write(f"{date_str},{time_str},{o:.5f},{h:.5f},{l:.5f},{c:.5f},0\n")
                    f.flush()

                # start new minute
                cur_minute = minute_ts
                if last_mid is not None:
                    o = h = l = c = last_mid
                    have_price = True
                else:
                    o = h = l = c = None
                    have_price = False
                continue

            # still same minute: update OHLC if we have a mid
            if mid is not None:
                if not have_price:
                    o = h = l = c = mid
                    have_price = True
                else:
                    c = mid
                    if mid > h: h = mid
                    if mid < l: l = mid

    except KeyboardInterrupt:
        # finalize the open minute on exit if we have one
        if cur_minute is not None and have_price:
            date_str = cur_minute.strftime('%Y.%m.%d')
            time_str = f"{cur_minute.hour}:{cur_minute.minute:02d}"
            f.write(f"{date_str},{time_str},{o:.5f},{h:.5f},{l:.5f},{c:.5f},0\n")
            f.flush()
        print("\nStopped.")
    finally:
        f.close()
        # ib.cancelMktData(ticker)  # uncomment if you want to explicitly cancel


def stream_eurusd_ticks_live():
    """
    Event-driven EURUSD streaming: prints each tick as it arrives.
    Requires real-time market data (use ib.reqMarketDataType(1)).
    Ctrl+C to stop.
    """
    # Ensure you're on real-time (1). Remove this if you intentionally want delayed.
    ib.reqMarketDataType(1)

    contract = Forex('EURUSD')
    ib.qualifyContracts(contract)

    # Subscribe to live market data (no snapshot)
    ticker = ib.reqMktData(contract, '', False, False)

    def on_update(tk):
        bid, ask = tk.bid, tk.ask
        t = tk.time or datetime.now(timezone.utc)
        t = t.astimezone(timezone.utc) if t.tzinfo else t.replace(tzinfo=timezone.utc)
        if bid is not None and ask is not None:
            mid = (bid + ask) / 2.0
            print(f"{t:%Y-%m-%d %H:%M:%S}  Bid={bid:.5f}  Ask={ask:.5f}  Mid={mid:.5f}")

    # Fire on every incoming tick update
    ticker.updateEvent += on_update

    print("Streaming EURUSD ticks (event-driven)… Ctrl+C to stop")
    try:
        ib.run()  # start the event loop; prints happen as ticks arrive
    except KeyboardInterrupt:
        print("\nStopped streaming.")
    finally:
        ib.cancelMktData(ticker)
        ticker.updateEvent -= on_update
