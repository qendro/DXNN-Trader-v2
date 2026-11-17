"""Stale-quote periods in the bars (SPEC.md section 5a).

A stale run is 6 or more consecutive H1 bars whose mid range is below 20% of the median H1 range of the previous 480
H1 bars (about 20 trading days). The excluded interval is the run plus the 24 hours after it, while indicators that
read the stale bars recover. Holiday quiet hours are caught by the same rule; that is intended.

Output: derived/ta_stage1/quality/<PAIR>.json = {"rule": ..., "intervals": [[start_utc, end_utc, "stale Nh"], ...]}.
Reads bar ranges only; computes nothing about future prices.
"""

import argparse
import datetime as dt
import json

import numpy as np

import common

MIN_RUN = 6
RATIO = 0.2
LOOKBACK = 480
AFTER_S = 24 * 3600
QUALITY_DIR = common.DERIVED_DIR / "quality"
RULE = (f">= {MIN_RUN} consecutive H1 bars with mid range < {RATIO} x median range of the previous {LOOKBACK} H1 bars; "
        f"excluded from run start to run end + 24 h")


def stale_intervals(h1):
    rng = h1["mid_high"] - h1["mid_low"]
    n = len(rng)
    med = np.full(n, np.nan)
    for i in range(100, n):
        med[i] = np.median(rng[max(0, i - LOOKBACK):i])
    flat = rng < RATIO * med  # NaN comparisons are False
    out, i = [], 0
    while i < n:
        if flat[i]:
            j = i
            while j < n and flat[j]:
                j += 1
            if j - i >= MIN_RUN:
                out.append([int(h1["start"][i]), int(h1["end"][j - 1]) + AFTER_S, f"stale {j - i}h"])
            i = j
        else:
            i += 1
    return out


def excluded_mask(available, intervals):
    """Boolean array: True where a time falls inside any excluded interval."""
    m = np.zeros(len(available), dtype=bool)
    for s, e, _ in intervals:
        m |= (available > s) & (available <= e)
    return m


def load(pair):
    with open(QUALITY_DIR / f"{pair}.json") as f:
        return json.load(f)["intervals"]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--pairs", default=",".join(common.PAIRS))
    args = ap.parse_args()
    QUALITY_DIR.mkdir(parents=True, exist_ok=True)
    for pair in args.pairs.split(","):
        h1 = dict(np.load(common.BARS_DIR / pair / "H1.npz"))
        iv = stale_intervals(h1)
        with open(QUALITY_DIR / f"{pair}.json", "w") as f:
            json.dump({"pair": pair, "rule": RULE, "bars_sha256": common.sha256_file(common.BARS_DIR / pair / "H1.npz"),
                       "intervals": iv}, f, indent=1)
        print(pair, len(iv), "intervals")
        for s, e, why in iv:
            print("   ", dt.datetime.utcfromtimestamp(s), "->", dt.datetime.utcfromtimestamp(e), why)


if __name__ == "__main__":
    main()
