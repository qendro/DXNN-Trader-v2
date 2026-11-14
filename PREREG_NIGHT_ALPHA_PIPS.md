# Night arm under `alpha_pips_t`: what counts as success (written 2026-11-14, before the run)

Written and committed before the run started, so the result can't be read into it afterwards. Change it only
before the first 2017 replay is looked at, and record any change in git.

## The run

- `DXNN_TEST_ARM=night DXNN_EPISODE_SEED=1 DXNN_EVOLUTION_SEED=1` (REVIEW.md 8.24, with `alpha_pips_t` per 8.26).
  Four families of 48, 20 + 40 + 4 x 45 generations, six chained runs.
- Machine: one GCP VM, 12 vCPUs (`n2-custom-12-24576`, x86-64, standard provisioning), OTP 28.4.1 with the JIT,
  `+scl false`. Fleet file `AWS-Deployment/config/gcp-local-fleet-night.yml`. A generation takes about twice as long as
  on the Mac, so the wall-time cap of runs 3 to 6 is 4 h (was 100 min) and every run gets its full generation count.
- Price table: `fx_tables/EURUSD1.txt`, SHA-256 `e3047acf460670c4e8430a0eeb1af3d53755950da661fb65de99fd1264e7414d`.
  `EURUSD_M1_MIDPOINT_UTC.txt` is not uploaded.
- The previous night (8.25, under `alpha_t`) is kept in `nights/2026-11-13_night_alpha_t/`.

## Pass rule (decides whether anything goes to 2018–2020)

1. **Candidates are chosen without 2017.** Per family, the one agent with the highest stored fitness in the final
   population (the one ending `_run6`). Four candidates, no more. Agents picked by looking at 2017 do not count.
2. **2017 replay.** `resim` of each candidate on all 369 episodes of 2017 at 0.25 pip, night settings (rollover rule
   on, fill now).
3. **A candidate passes** when, on those 369 episodes:
   - its mean money per episode after cost is above zero with t = mean / standard error > 2, and
   - its mean money is above each of the flat, long, momentum and time-of-day baselines on the same episodes.
4. **Only a passing candidate goes to the single 2018–2020 test**, once. If none passes, 2018–2020 stays unread.

With four candidates, t > 2 gives roughly a one-in-ten chance that at least one passes by luck. That is accepted
because 2017 is a gate, not the result; the 2018–2020 test is the result.

## Warning signs during the run (read in the morning, in this order)

1. `rho_score_money` per family. If it falls near zero or below while the family's score rises, the agents have
   found a new way to game the score: stop and look at what they do before reading anything else.
2. `survivor_rescore` and `net_pips_ep`: whether selection keeps something that holds on new episodes.
3. Validation money at the end of each run, against the baselines on the same line.
4. `score_money_check` on the run-4 and run-6 populations, then the 2017 `resim` above.

## What does not count

- A score that rises while money doesn't.
- A family that beats the baselines on training episodes only.
- Any agent chosen after seeing its 2017 money.
