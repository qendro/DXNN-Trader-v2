# What a EUR/USD position really costs, 2012 to 2017: the evidence

Written 2026-11-13. A research note only: no code was changed, nothing was run, and nothing was downloaded. It sets the simulator's flat 0.25 pip per position against published evidence. The project's own figures come from REVIEW.md, sections 8.12, 8.15 and 8.21.

**How to read it.** Each section separates **Sources state** (attributed, with short quotes only) from **Inference** (mine). Broker and vendor pages are labelled *(broker)* or *(vendor)*. They are interested parties and seldom say how they measured.

**Units.** 1 pip = 0.0001 USD per EUR. "Per position" means a round trip: open, then close or reverse. The simulator charges 0.25 pip per position and counts a reversal once. That amounts to assuming that **the full spread plus two commissions comes to 0.25 pip**. A reversal sells (or buys) two units: it pays half a spread to close one position and half a spread to open the next. So one charge per position equals one full spread per position.

**Clock.** The file runs on London time, and 22:00 on it is 17:00 New York. That holds except for about three weeks in March and one in late October to early November, when the two cities' summer-time dates differ. In those weeks 17:00 New York is 21:00 London.

---

## 0. Summary

| Question | Short answer | Strength of evidence |
|---|---|---|
| Quoted spread, liquid hours, 2012–2017 | Interbank (EBS) minimum price step was 0.5 pip from September 2012, so the quoted spread on EBS was at least 0.5 pip. Composite indicative quotes (Olsen) had a median of 1.4 pips in April 2013 and 0.5 in April 2016. Retail ECN raw spread was about 0.3 pip in peak hours in 2015 (FXCM, broker) | Medium |
| Spread outside liquid hours | Widest 18–22 GMT, peaking at 21–22 GMT (EBS 1999–2001). FXCM 2015 off-peak average was 1.9× its peak average | Shape: medium. Size for 2012–2017: weak |
| At the 17:00 New York rollover | Industry research and an exchange note call it the least liquid time of day. The only figures (2.5–5 pips for a few minutes, back to normal by 18:00 New York) come from a broker page that gives no data | Size: weak |
| Around data releases | Spreads widest within a minute of the release. High-frequency traders widen their spread by more than 30% in the minute before (FCA, 2012–2015 data). Price adjusts within minutes | Medium |
| Spread against recent volatility | Positive relation, well documented. A published form: spread proportional to volatility per trade (equities, R² above 0.9). No FX coefficient found for 2012–2017 | Form: medium. Coefficient: none |
| Commission | Interactive Brokers 0.20 basis point of value per side, about 0.44–0.54 pip per round trip. Dukascopy $35 per million per side, about 0.74–0.95 pip per round trip. LMAX institutional taker $2.5–5 per million per side, about 0.06–0.14 pip per round trip | Medium (schedules) |
| Realistic all-in cost per position, liquid hours | Institutional taker 0.2–0.6 pip. Retail ECN 0.5–1.0 pip. Retail no-commission account about 1–2 pips | Medium |
| **Central case among 0.25 / 0.5 / 1** | **0.5 pip** for liquid hours. 0.25 is the optimistic floor. 1 pip is the stress case, and the central case for a retail account. For these agents, which trade disproportionately at the rollover and after spikes, the hour-weighted average is above 0.5 | Inference |

---

## 1. Level of the spread, 2012–2017

### Sources state

- **Composite indicative quotes (Olsen), median EUR/USD spread in April of each year.** 2.0 pips in 2010, 1.4 in 2013 and 0.5 in 2016. In relative terms that is 1.47, 1.07 and 0.44 basis points. Source: Hasbrouck & Levich, *FX Liquidity and Market Metrics: New Results Using CLS Bank Settlement Data*, NYU Stern working paper (2019), Table 5; also NBER WP 23206 (2017). The authors say these are "indicative prices", "not necessarily firm" and not necessarily "the best bid and offer available to any participant". The sample is the first quote in each ten-second window. They also find price impact for larger orders improved from 2010 to 2013 and worsened from 2013 to 2016, while spreads kept falling. https://pages.stern.nyu.edu/jhasbrou/Research/Hasbrouck-Levich%20v25.pdf
- **EBS minimum price step.** EBS moved the majors to tenth-of-a-pip pricing in early 2011. It reversed to half-pip pricing in EUR/USD and 22 other pairs on 17 September 2012 (trade press: fx-markets.com "EBS reversal on decimals brings relief among traders"; LeapRate "ICAP-EBS goes to half-pip pricing"; Mondo Visione). Under half-pip pricing the last digit can only be 0 or 5. A Financial Markets Group / arXiv study (arXiv 1307.5440) found that spreads narrowed after decimalisation but that market quality improved after the move to half pips.
- **EBS median spread in one hour.** The CME/EBS Market Profile help page says that in EUR/USD between 10:00 and 11:00 the "median spread was 1 full pip" *(vendor; seen only as a search snippet; the page timed out; period and time zone not stated)*.
- **Retail ECN, 2015.** FXCM reported an EUR/USD average of 0.292 pip in peak hours (01:00–13:00 New York) and 0.553 off-peak for 2015. 75% of its EUR/USD volume traded in peak hours. *(Broker press release, Sept 2016, via FinanceFeeds and Finance Magnates; it is not stated whether commission was charged on top.)*
- **Retail ECN, recent.** QuantRocket measured Interactive Brokers one-minute bid and ask over one month: EUR/USD spread 0.1–0.2 pip *(vendor blog; recent, not 2012–2017; its own unit conversion is inconsistent)*.
- **Retail no-commission accounts.** No reliable source for 2012–2017 was found. Current comparison sites put Oanda at about 0.9 pip (BrokerChooser) *(commercial comparison sites)*.

### Inference

- From September 2012 on, a taker on EBS paid at least 0.5 pip per round trip in spread (at least 0.25 each way), even when the book was at its tightest. Most dealer-to-client streams and ECNs quoted inside EBS, so 0.1–0.3 pip raw spreads in liquid hours at retail ECNs (FXCM 2015, Dukascopy's advertised "from 0.1") are consistent with this.
- **By year.** Spreads fell over the period. The Olsen series falls by a factor of 2.8 from 2013 to 2016, but it is indicative and composite, so it probably overstates the decline at a single executable venue. A modest year factor is defensible: about 1.3 for 2012–2013, 1.15 for 2014–2015 and 1.0 for 2016–2017. No executable-quote series by year was found to pin this down.
- **Range for 2012–2017, liquid hours, full spread:** institutional 0.1–0.5 pip, retail ECN raw 0.1–0.4 pip, retail no-commission 0.8–2 pips (weakly sourced).

---

## 2. How the spread varies within the day

### Sources state

- **Hour of day, EBS firm quotes, 1999–2001.** For euro/dollar, "the bid-ask spread is narrow during the London business hours. It becomes wider after Hours 16-17 and peaks at Hours 21-22" (GMT). The largest spreads come when activity is lowest, "GMT 18 to 22 hours". Activity has troughs at the Tokyo lunch, the London lunch and 21–22 GMT. In the opening hour of Tokyo (GMT 0) the spread is wider than in other Tokyo hours despite high volume. Across hours, spread and volatility are negatively correlated, because the busy hours are both the most volatile and the tightest. Source: Ito & Hashimoto, NBER WP 12413 (2006), published in *J. Japanese and International Economies*. https://www.nber.org/papers/w12413
- **Activity pattern, 2010–2016.** CLS settlement activity peaks in Tokyo, London and New York hours, and "liquidity varies considerably over the 24-hour trading day" (Hasbrouck & Levich 2019).
- **Peak against off-peak, 2015.** FXCM's off-peak window (13:00–01:00 New York, roughly 18:00–06:00 London) averaged 0.553 pip against 0.292 in peak hours: a ratio of 1.9 *(broker)*.
- **17:00 New York rollover.**
  - BidFX research found a "strikingly low level of FX trading" between 16:00 and 18:00 New York, "leading to widening spreads and shrinking liquidity among G10 currency pairs" *(trade press, Global Trading; paywalled; no figures seen)*. FinanceFeeds attributes part of this to bank pricing engines restarting at 17:00.
  - CME Group (2026) calls the hour after 17:00 New York "often the least liquid hour of the FX day", because dealers cut risk into the book close and CME FX futures pause for that hour *(exchange explainer)*.
  - Figures appear only on a broker blog (FXNX, Saint Lucia, 2026): EUR/USD 2.5–5.0 pips from 17:00 to 17:05 New York against a baseline of about 0.2. Spreads fall 50–60% from the peak by 17:06–17:20, reach 1.2–1.5× baseline by 17:45 and return to baseline by 18:00. The page cites no data *(broker; treat as anecdote)*.
- **16:00 London fix (WM/Reuters).** Using inter-dealer order book data for 2012 and 2017, Evans, O'Neill, Rime & Saakvitne (FCA Occasional Paper 46, 2018) find that quoted spreads during the fix fell after 2013. After the fix window was lengthened to five minutes in 2015, "quoted spreads and price impact increase", and order book depth fell. No EUR/USD magnitudes were obtained (the PDF could not be read here). https://www.fca.org.uk/publications/occasional-papers/occasional-paper-no-46-fixing-fix-assessing-effectiveness-4pm-fix
- **Scheduled data releases.**
  - Huang, O'Neill, Ranaldo & Yu (FCA Occasional Paper 63, 2023; spot FX 2012–2015): high-frequency traders pull back before scheduled announcements, and in the minute before, their bid-ask spread widens "by over 30%". Dealer liquidity is more resilient. https://www.fca.org.uk/publications/occasional-papers/op-63-hfts-dealer-banks-liquidity-fx-trading
  - Tham (2008, conference paper): the widest quoted spreads are "within a minute of announcements".
  - Chaboud, Chernenko, Howorka, Iyer, Liu & Wright (Fed IFDP 823, 2004; EBS): prices react within a few minutes, while volume rises even on releases that match expectations and stays elevated longer.
  - An FXStreet survey of more than 90 brokers around US payrolls (2015) found spreads about five times normal at the peak, recovering gradually *(practitioner)*.
- **Tokyo and London opens.** Apart from the wider Tokyo opening hour above, no source found measures a spread jump at the London open (08:00 London) for EUR/USD.

### Inference

- **Shape.** Narrowest from about 08:00 to 17:00 London. Wider through the New York afternoon. Widest from about 21:00 to 23:00 London, with a spike in the first minutes after 22:00 (17:00 New York). Moderately wide through the Asian session.
- **Duration.**
  - Rollover: a severe spike of about 5–10 minutes, elevated for about 60 minutes. Only the broker page gives this, but it fits the exchange and BidFX descriptions.
  - Data releases: a severe spike from about one minute before to one or two minutes after, elevated for roughly 5–15 minutes. Price adjusts within minutes (IFDP 823); the volume tail is longer.
  - Fix: no evidence of a multiple-of-normal widening. It is one of the most liquid moments of the day.
- **Fit with the project's measurements.** REVIEW.md 8.12 and 8.15 find a disproportionate share of profit in the two hours around 22:00, in the hour of US releases (13:30 London) and just after fast moves. Those are exactly the moments where the sources say the spread is a multiple of its daytime level.

---

## 3. Spread and recent volatility

### Sources state

- Bollerslev & Melvin (1994, *J. International Economics*; Deutschemark/dollar quotes, April–June 1989): "the spread size is positively related to underlying exchange rate uncertainty". Volatility is measured with a GARCH model and the spread is modelled with an ordered probit, because spreads take discrete values.
- Wyart, Bouchaud, Kockelkoren, Potters & Vettorazzo (2008, *Quantitative Finance*; equity order books): spread ≈ c × (volatility per trade), "with R2s exceeding 0.9". The link is to volatility per trade, "rather than with the volatility per unit time". https://arxiv.org/abs/physics/0603084
- Huang et al. (FCA OP63): high-frequency traders' liquidity is less sensitive to market-wide volatility spikes than dealers'.
- Ito & Hashimoto (above): across hours of the day the correlation is negative. That is a time-of-day effect, not the conditional relation.

### Inference

- **No published coefficient** links the EUR/USD spread in 2012–2017 to the previous minute's range. The two findings are compatible: conditional on the hour, more volatility means a wider spread; across hours, busy hours are both volatile and tight. A model should therefore apply the volatility term *relative to the hour's normal range*, or as a small additive term on top of the hour schedule.
- The Wyart et al. form implies that a 1-minute range ten times its normal size, without a matching rise in trade count, means a spread several times wider. That is consistent with the broker anecdotes of 5× spreads at payroll releases.

---

## 4. Total cost per round trip, including commission

### Sources state (commission schedules)

| Venue | Schedule | Source | Status for 2012–2017 |
|---|---|---|---|
| Interactive Brokers | 0.20 basis point × trade value per side (up to $1 billion a month), minimum $2 per order | IB pricing pages (current) | Not confirmed for 2012–2017 |
| Dukascopy | $35 per $1 million per side (deposit under $5,000), $33 and $30 for the next tiers. Volume schemes "took effect on 1 July 2012" | Dukascopy tariffs PDF | In force from July 2012 per the document |
| LMAX (institutional MTF, taker) | $5 per million ($0–2 billion a month) down to $2.50 | LMAX schedule via search snippet *(venue)* | Undated |

Conversion: a commission of b basis points of USD value per side costs b × P pips per side, where P is the EUR/USD rate (about 1.30–1.39 in 2012–2014 and 1.04–1.20 in 2015–2017).

- Interactive Brokers: 0.22–0.27 pip per side, **0.44–0.54 pip per round trip**.
- Dukascopy: 0.37–0.47 pip per side, **0.74–0.95 pip per round trip**.
- LMAX taker: **0.06–0.14 pip per round trip**.

### Inference: all-in cost per position (spread plus commissions), liquid hours

| Participant | Spread | Commission | All-in per position |
|---|---|---|---|
| Institutional taker (EBS, LMAX, bank stream) | 0.1–0.5 | 0.05–0.15 | **0.2–0.6 pip** |
| Retail ECN, Interactive Brokers type | 0.1–0.3 | 0.44–0.54 | **0.55–0.85 pip** |
| Retail ECN, Dukascopy type | 0.1–0.4 | 0.74–0.95 | **0.85–1.35 pip** |
| Retail no-commission account | 0.8–2 | 0 | **0.8–2 pips** (weakly sourced) |

**Against 0.25 pip.** Only an institutional taker in the most liquid hours reaches 0.25, and the EBS half-pip step (from September 2012) makes even that hard on the main inter-dealer venue. For any retail-accessible account, 0.25 is about half the real cost in the best hours. These figures exclude last-look rejections. LMAX's own research (an interested party, 2016 data) puts the cost of trading against last-look liquidity at up to $48.86 per million, about 0.5 pip. Rejections are most likely just after fast moves, which is where these strategies trade.

---

## 5. What published intraday FX backtests assume

### Sources state

- Neely & Weller (2003, *J. International Money and Finance*; intraday rules chosen by genetic programming and linear models): once "realistic transaction costs and trading hours" are included, there are no excess returns. Results are "very sensitive" to cost and liquidity. A 2014 St. Louis Fed appendix summarising this literature gives one-way costs of 3 basis points in the 1990s and "a minimum of one basis point" (about 1.2–1.4 pips one way at 2012–2017 rates).
- A QuantConnect implementation of a time-of-day EUR/USD strategy (Jiang, SSRN 2613592) assumes "1pip" bid/ask spread *(code comment; the paper itself not checked)*.
- QuantRocket's EUR/USD time-of-day study used Interactive Brokers one-minute bid and ask. It modelled 0.1 basis point slippage plus 0.2 basis point commission, and found that "even 1 basis point will destroy the profitability" *(vendor blog)*.
- A thesis on ruble intraday rules (Erasmus) found that real rather than estimated costs "wipes away most of the profits".
- On latency, the BIS Quarterly Review (December 2016) describes venue "speed bumps" of "randomised pauses (measured in milliseconds)". EBS and others introduced these, so a fill at the bar's displayed price is not guaranteed even with a fast connection.

### Inference

- Careful studies fill at the bid or ask, not the midpoint, and either delay the fill or restrict trading hours. Filling at the midpoint close of the bar that generated the signal has two problems. It assumes the decision and the fill happen at the same instant. And it captures the bid-ask bounce in the midpoint as if it were tradable. REVIEW.md 8.21 measures this: acting one bar late takes about half the agents' profit.
- Typical assumptions in academic work are 1 basis point or more one way. That is about 2–3 pips per round trip in 2012–2017, much larger than 0.25, though it mostly reflects older decades. Recent practitioner work with real quotes uses about 0.3–0.6 pip per round trip all-in for a retail ECN, and finds intraday edges disappear near 1 basis point.

---

## 6. Free or low-cost EUR/USD bid and ask data, 2012–2017

| Dataset | Bid and ask? | Resolution and coverage | Format | Licence | Known problems | Size for EUR/USD 2012–2017 |
|---|---|---|---|---|---|---|
| **Dukascopy** (Swiss bank, SWFX ECN) | Yes. "Tick data always consists of both bid and ask prices" (Dukascopy forum staff). Volumes are liquidity at the best quotes, not trades | Ticks. EUR/USD from 2003 per common use; one article says April 2007 "for most currencies" (unverified for EUR/USD). Covers 2012–2017 | One file per hour, LZMA-compressed binary (.bi5), via a public URL feed. CSV from the web tool or the JForex Historical Data Manager | Dukascopy terms; not reviewed here | Reported gaps (e.g. EUR/USD 2012, forum). An old bug with website timestamps at GMT+2. Bars from different Dukascopy tools disagree (forum, 2018). Quotes are Dukascopy's own venue, not inter-dealer | Not established. Estimate (inference): 1–3 GB compressed ticks; about 6 × 52 × 120 ≈ 37,000 hourly files |
| **TrueFX** (Integral FX grid) | Yes, top of book | Ticks, 15 pairs, from May 2009 | Monthly zipped CSV; free registration | Registration terms; not reviewed | No volume. Aggregated bank streams, indicative of a top-tier institutional book | EUR/USD archive about 866 MB per an R download script (year span not stated) |
| **HistData.com** | Tick files: yes. M1 Generic ASCII files: **bid only** (HistData specification page) | Ticks; a February 2012 example file exists; full coverage not stated | CSV ("Generic ASCII"), monthly files; timestamps in New York standard time with no summer-time change | Not stated on the specification page | Origin of quotes not stated | Not established |
| **FXCM** | Candles reportedly carry bid and ask OHLC (not confirmed on the GitHub page). Ticks have bid and ask | GitHub page now lists m1 candles for **2017–2020** only. Tick data was once public by week (docs example `tickdata.fxcorporate.com/EURUSD/2015/1.csv.gz`); now "email premiumdata@fxcm.com" | Weekly gzip CSV, UTC | "For personal use", FXCM EULA *(broker)* | Coverage of 2012–2016 uncertain | Not established |
| **Gain Capital (ratedata.gaincapital.com)** | Yes (CSV: id, dealable flag, pair, time, bid, ask) | Weekly ticks from the 2000s | Zipped CSV | Not found | Forum reports of early gaps and spikes; current availability unknown | Not established |
| **Interactive Brokers** (project already has access) | BID_ASK bars | How far back one-minute bid/ask bars reach for 2012–2017 is not established | API | Account terms | Recent years certainly available (REVIEW.md 8.12 proposes it) | — |
| Paid, for reference | Tick Data Inc. (from May 2008), Olsen, Refinitiv tick history, EBS Data Mine | | | Commercial | | |

### Proposed downloads (for the user to approve; none made)

1. **Dukascopy EUR/USD, ticks or one-minute bid and ask candles, 2012-01 to 2017-12.** The main calibration set: hour-of-day spread profile, the rollover spike and its duration, the spread after large one-minute moves. Probably 1–3 GB compressed if ticks; much less as one-minute bid and ask candles from the JForex Historical Data Manager.
2. **TrueFX EUR/USD monthly tick files, 2012–2017** (72 zips; size uncertain, under 1 GB per the R script). A second, more institutional venue to check that the *shape* found in item 1 is not specific to Dukascopy.
3. Optional: **HistData EUR/USD tick files** for the same years, as a third check.
4. Optional, already proposed in REVIEW.md 8.12: **Interactive Brokers one-minute BID_ASK bars** for the years IB will serve, to compare with the project's IB midpoint file.

A caution for all of them: they are dealer or ECN quotes, not EBS. Use them for the *shape* (multipliers by hour and against the previous range) and set the *level* from the commission schedule and the spread range in section 4.

---

## 7. Cost by hour of the London clock

Round-trip cost per position, in pips, for a **retail ECN of Interactive Brokers type**. The low column is an institutional taker. Daytime values follow section 4. Other hours are daytime spread × hour multiplier, plus commission:

- low: spread 0.1 + commission 0.1
- central: spread 0.2 + commission 0.3
- high: spread 0.4 + commission 0.5

**Key to sources:**

- IH = Ito & Hashimoto (EBS 1999–2001): direction and shape only, no 2012–2017 size
- FX15 = FXCM 2015 *(broker)*: peak against off-peak ratio of 1.9
- RO = rollover sources (BidFX via trade press; CME note; FXNX *(broker)* for sizes)
- OP63 = FCA Occasional Paper 63
- OP46 = FCA Occasional Paper 46
- I = inference

| London hour | Market context (winter; in summer the Asian hours shift with summer time) | Spread multiplier low / central / high | Cost low | Cost central | Cost high | Source |
|---|---|---|---|---|---|---|
| 00 | Tokyo open (09:00 Tokyo, winter) | 1.5 / 2 / 3 | 0.25 | 0.70 | 1.70 | IH (Tokyo first hour wider), FX15 |
| 01–05 | Tokyo and Singapore session; Tokyo lunch about 03 | 1.3 / 1.8 / 2.5 | 0.23 | 0.66 | 1.50 | IH, FX15 |
| 06–07 | Before London; Frankfurt from 07 | 1.1 / 1.4 / 2 | 0.21 | 0.58 | 1.30 | FX15 (off-peak ends 06:00), I |
| 08 | London open | 1 / 1.1 / 1.5 | 0.20 | 0.52 | 1.10 | No direct evidence of widening found; I |
| 09–11 | London morning; euro-area releases (09:00–10:00) | 1 / 1 / 1.3 | 0.20 | 0.50 | 1.02 | IH (narrowest), FX15; releases OP63 |
| 12 | London lunch | 1 / 1.1 / 1.4 | 0.20 | 0.52 | 1.06 | IH (activity trough) |
| 13 | US releases (13:30 in winter, 12:30 in the summer-time gap weeks) | 1 / 1.3 / 2 (hour average; the release minute is 3–5×) | 0.20 | 0.56 | 1.30 | OP63, Tham 2008, FXStreet survey *(practitioner)* |
| 14–15 | London and New York overlap; US releases at 15:00 | 1 / 1 / 1.3 | 0.20 | 0.50 | 1.02 | IH, FX15 |
| 16 | WM/Reuters fix (16:00) | 1 / 1 / 1.3 | 0.20 | 0.50 | 1.02 | OP46 (spreads up after 2015, size unknown) |
| 17 | London close | 1 / 1.2 / 1.6 | 0.20 | 0.54 | 1.14 | IH ("wider after Hours 16-17") |
| 18–19 | New York afternoon; FOMC 19:00 on eight days a year | 1.3 / 1.8 / 2.5 | 0.23 | 0.66 | 1.50 | IH (18–22 GMT widest), FX15 |
| 20–21 | Late New York | 1.5 / 2.2 / 3 | 0.25 | 0.74 | 1.70 | IH, RO (BidFX 16–18 New York) |
| **22** | **17:00 New York rollover** | **2 / 4 / 10** (hour average; first 5–10 minutes 10–25× daytime) | 0.30 | **1.10** | **4.50** | RO, IH (peak 21–22 GMT); sizes only from a broker page |
| 23 | Recovery; Sydney and Wellington only | 1.5 / 2.5 / 4 | 0.25 | 0.80 | 2.10 | RO (FXNX: baseline by 18:00 New York), IH, I |

Weekend open (Sunday about 22:00 London) and holidays: **no evidence found**. Assume at least the hour-22 values.

---

## 8. A cost model the simulator can use

Inputs: one-minute midpoint OHLC and the clock. Charge **half the spread at each fill**, at the time of that fill, plus commission per side:

```
half_spread(t) = 0.5 * s0 * Y(year) * M(hour_london(t)) + k * max(0, R(t-1) - Rbar(hour)) [+ rollover spike]
cost of a fill = half_spread(t) + c_side
reversal at t  = 2 * half_spread(t) + 2 * c_side     (one close + one open; matches "counted once" when c_side is small)
```

- `R(t-1)` is the previous bar's high minus low, in pips (known when the decision is made).
- `Rbar(hour)` is the median one-minute range for that hour, computed once from the training years. This keeps the volatility term from double-counting the hour effect (section 3).
- Rollover spike: optionally multiply `M` by an extra 3 for minutes 22:00–22:09. That puts the spike in the first minutes rather than spread over the hour.

| Parameter | Central | Low | High | Basis |
|---|---|---|---|---|
| `s0`, daytime full spread (pips) | 0.2 | 0.1 | 0.4 | Section 1: FXCM 2015 0.29 peak; EBS step 0.5; ECN "from 0.1". I |
| `c_side`, commission per side (pips) | 0.15 | 0.05 | 0.27 | Section 4 schedules. I |
| `M(hour)` | Central column of section 7 | Low column | High column | IH, FX15, RO. I |
| `Y(year)` | 1.3 (2012–13), 1.15 (2014–15), 1.0 (2016–17) | 1.0 throughout | 2.0, 1.5, 1.0 | Hasbrouck & Levich Table 5 (Olsen, indicative). I |
| `k`, extra half-spread per pip of excess previous range | 0.03 | 0.01 | 0.08 | **No published FX coefficient.** Set so that a previous-minute range 10 pips above normal adds about 0.3 pip to a fill (central), roughly matching "about five times normal" at payrolls. Wyart et al. for the form. I |
| Cap on half_spread | 3 pips | 1.5 | 10 | I |

**What the model implies (central parameters, `Y` = 1).**

- Daytime reversal (one position): 0.2 + 0.3 = **0.5 pip**.
- Reversal at 22:05 with the spike: 0.2 × 4 × 3 + 0.3 ≈ **2.7 pips**.
- Reversal at 13:31 just after a 15-pip release minute (normal range about 2): 0.2 × 1.3 + 2 × 0.03 × 13 + 0.3 ≈ **1.3 pips**.

**What to calibrate first if a download is approved.** `M(hour)` and the rollover spike from Dukascopy and TrueFX spreads by minute of the London clock. Then `k`, from a regression of the spread at minute t on the previous minute's range, within hour. Keep `s0` and `c_side` from the schedules, because ECN spread levels do not transfer to other venues.

---

## 9. Which central case: 0.25, 0.5 or 1 pip?

- **0.25 pip** is supported only as an optimistic lower bound: an institutional taker in the most liquid hours. Even that is below the EBS half-pip quoted spread that applied from September 2012.
- **0.5 pip** is the best-supported **central case for liquid hours** in 2012–2017: a retail ECN round trip (0.1–0.3 spread plus 0.44–0.54 Interactive Brokers commission) or an institutional taker at EBS-level spreads.
- **1 pip** is the plausible stress case. It is also the central case for a retail no-commission account or a Dukascopy-type commission schedule, and close to what academic backtests assume (1 basis point or more one way).

For the strategies in REVIEW.md, the flat number understates the cost by more than the ratio 0.25 : 0.5. They are always in the market and trade disproportionately at hour 22, at release minutes and after spikes. Under the central schedule of section 7, their average cost per position would be about 0.6–0.8 pip (inference, from their 2–3% share of entries in hour 22 and the hour weights). REVIEW.md 8.12 and 8.15 find the edge is gone at 0.5–0.6 pip.

---

## 10. What could not be established

1. **EUR/USD spread by year for 2012–2017 from an executable venue** (EBS, Reuters Matching, LMAX). Only Olsen indicative medians for April 2013 and 2016, and the EBS minimum step, were found.
2. **Rollover magnitudes and duration from a primary dataset.** Industry research (BidFX) is paywalled and gave no figures. The only numbers are from a broker blog with no stated data.
3. **The size of spreads at the 16:00 fix and around US and euro-area releases for EUR/USD.** OP46 and OP63 give directions and one percentage (more than 30%, high-frequency traders, the minute before). Their PDFs could not be read here.
4. **A published regression of FX spread on the previous minute's range for 2012–2017.** Only the general form (Bollerslev & Melvin 1994; Wyart et al. 2008) was found. `k` above is a placeholder.
5. **Any evidence of widening at the London open** for EUR/USD.
6. **Retail no-commission spreads by year** (Oanda, FXCM standard, IG). No archive found. Oanda's historical spreads tool may have them.
7. **Whether Interactive Brokers' commission schedule in 2012–2017 matched today's**, and how far back IB serves one-minute bid/ask bars.
8. **Dataset sizes and licences** for Dukascopy, HistData and FXCM, and whether FXCM's 2012–2016 files are still public.
9. **Last-look rejection rates** for a retail or institutional taker trading right after fast moves (only LMAX's interested-party estimate was found).
10. The CME/EBS "median spread 1 full pip" statement was seen only as a search snippet. Its period and time zone are unknown.

## Sources

- Hasbrouck & Levich (2019), *FX Liquidity and Market Metrics* — https://pages.stern.nyu.edu/jhasbrou/Research/Hasbrouck-Levich%20v25.pdf ; NBER WP 23206 — https://www.nber.org/papers/w23206
- Ito & Hashimoto (2006), NBER WP 12413 — https://www.nber.org/papers/w12413
- Evans, O'Neill, Rime & Saakvitne (2018), FCA OP46 — https://www.fca.org.uk/publications/occasional-papers/occasional-paper-no-46-fixing-fix-assessing-effectiveness-4pm-fix
- Huang, O'Neill, Ranaldo & Yu (2023), FCA OP63 — https://www.fca.org.uk/publications/occasional-papers/op-63-hfts-dealer-banks-liquidity-fx-trading
- Chaboud et al. (2004), Fed IFDP 823 — https://www.federalreserve.gov/pubs/ifdp/2004/823/ifdp823.htm
- Bollerslev & Melvin (1994), J. International Economics 36 — https://econpapers.repec.org/RePEc:eee:inecon:v:36:y:1994:i:3-4:p:355-372
- Wyart et al. (2008), Quantitative Finance — https://arxiv.org/abs/physics/0603084
- Breedon & Ranaldo (2013), JMCB; SNB WP 2011-04 — https://www.snb.ch/en/publications/research/working-papers/2011/working_paper_2011_04
- Moore, Schrimpf & Sushko (2016), BIS Quarterly Review — https://bis.org/publ/qtrpdf/r_qt1612e.htm
- Neely & Weller (2003), JIMF — https://econpapers.repec.org/RePEc:fip:fedlwp:1999-016 ; St. Louis Fed WP 2014-033 appendix — https://fraser.stlouisfed.org/files/docs/publications/frbsl_wp/2014-033_appendix.pdf
- EBS half-pip: https://www.fx-markets.com/node/2203637 ; https://www.leaprate.com/news/icap-ebs-goes-to-half-pip-pricing ; arXiv 1307.5440 — https://ar5iv.arxiv.org/html/1307.5440
- BidFX research (trade press): https://www.globaltrading.net/?p=27059 ; https://financefeeds.com/bidfx-research-finds-liquidity-gap-in-fx-markets-between-4-pm-and-6-pm-est/
- CME Group, "When FX futures pause" (2026) — https://www.cmegroup.com/articles/2026/when-fx-futures-pause.html
- FXNX *(broker)* — https://fxnx.com/en/blog/spread-widening-rollover-hourly-cost-pattern-measured
- FXCM 2015 spreads *(broker)* — https://financefeeds.com/fxcm-averaged-0-3-pips-spread-on-eurusd-during-peak-hours-in-2015/ ; https://www.financemagnates.com/forex/brokers/fxcm-publishes-average-spreads-data-on-majors-for-2015/
- FXStreet payrolls broker survey *(practitioner)* — https://www.fxstreet.com/education/how-to-trade-nonfarm-payrolls-201508280000
- Interactive Brokers commissions — https://www.interactivebrokers.co.uk/en/pricing/commissions-spot-currencies.php
- Dukascopy tariffs — https://www.dukascopy.com/swiss/docs/bankFormsEU/Tariffs.pdf ; historical data — https://www.dukascopy.com/swiss/en/marketwatch/historical/ ; forum — https://www.dukascopy.com/swiss/english/forex/jforex/forum/viewtopic.php?p=4301
- LMAX schedule and last-look research *(venue)* — https://tradersunion.com/brokers/forex/view/lmax/fees-and-spread/ ; https://www.lmax.com/documents/LMAX-TCA-leaflet.pdf
- QuantRocket *(vendor)* — https://www.quantrocket.com/blog/business-day-forex-patterns ; QuantConnect Jiang replication — https://www.quantconnect.com/terminal/cache/embedded_backtest_1948bed1c108bf7be834ed86600ba0a6.html
- HistData specification — https://www.histdata.com/f-a-q/data-files-detailed-specification/
- TrueFX download script — https://rdrr.io/github/gsee/FinancialInstrument/src/inst/parser/download.TrueFX.R ; Waters Technology — https://www.waterstechnology.com/trading-tech/1615984/integral-packages-fx-feed-history
- FXCM market data *(broker)* — https://github.com/fxcm/MarketData
- Gain Capital data (forums) — https://tickstory.com/forums/topic/historic-rate-data-from-gain-capital/
- CME/EBS Market Profile help *(vendor; snippet only)* — https://cmegroup.com/tools-information/webhelp/quant-analytics/Content/EBSMarketProfile.html
