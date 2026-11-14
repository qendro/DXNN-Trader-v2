#!/usr/bin/env node
// Result-folder pipeline for the four-arm experiment and the chronological folds (REVIEW.md 8.23).
//
//   node analysis/pipeline.js <results_dir> [--out <dir>] [--n 8] [--train-k 900] [--conditions all|name,name]
//        [--work <dir>] [--timeout-s 600] [--mem-gb 3.5] [--parallel N] [--no-resim] [--target-pips 0.25]
//
// <results_dir> holds one folder per run (or is one): mnesia.nonode@nohost.backup, run.out,
// logs/Benchmarker/generation_summary.log, logs/exp_runner/exp_runner.log.
//
// Writes to <out> (default <results_dir>/analysis_out):
//   runs.csv           one row per run of each experiment: arm, fold, seeds, spans, cost, population, resim status
//   learning_curve.csv one row per GEN_SUMMARY line
//   curve_summary.csv  per run: first and last generation and the mean of the last 20 generations
//   validation.csv     one row per validation reading (GEN_SUMMARY lines with val_agents > 0) with the baselines of
//                      the same line, and the reversal rules' money on the same episodes from the re-simulation
//   chosen.csv         the best N agents of each re-simulated population, by the run's own ranking
//   per_agent.csv      re-simulation: one row per population, span, condition and agent
//   populations.csv    re-simulation: one row per population, span and condition (median over the N agents, the
//                      equal-weight portfolio of the N with day- and week-block standard errors and drawdown, and the
//                      six baselines on the same episodes)
//   comparison.csv     per outcome: every arm-seed value, arm means, paired differences across seeds, the two main
//                      effects and their interaction
//   folds.csv          one row per fold, arm and seed (and a mean row per fold and arm when there are several seeds)
//   summary.md         every number in it cites the CSV file and row_id it comes from
//
// Hard rules: a run whose training or validation span ends after 2017-12-31 is refused (the folder is not analysed and
// the exit code is 3); the Erlang side (dxnn_results.erl) refuses such spans and episodes again, and the VM only ever
// loads a price file cut before 2018. One VM at a time, erl +Bd +scl false, killed above --mem-gb or after --timeout-s.
'use strict';
const fs = require('node:fs');
const path = require('node:path');
const vm = require('./vm');

const LAST_DAY = '2017-12-31';
const ARM_ORDER = ['ff_level', 'rec_level', 'ff_vol', 'rec_vol'];
const RULES = ['rev60', 'rev240', 'flat', 'long', 'mom60', 'clock'];
const LAST_WINDOW = 20;

// ---------------------------------------------------------------------------------------------------- arguments
function parseArgs(argv) {
  const a = { n: 8, 'train-k': 900, conditions: 'all', 'timeout-s': 600, 'mem-gb': 3.5, 'target-pips': 0.25, resim: true };
  const pos = [];
  for (let i = 0; i < argv.length; i++) {
    if (argv[i] === '--no-resim') a.resim = false;
    else if (argv[i].startsWith('--')) a[argv[i].slice(2)] = argv[++i];
    else pos.push(argv[i]);
  }
  if (pos.length !== 1) throw new Error('usage: node analysis/pipeline.js <results_dir> [options]');
  a.results = path.resolve(pos[0]);
  a.out = path.resolve(a.out || path.join(a.results, 'analysis_out'));
  a.work = path.resolve(a.work || path.join(a.out, '_work'));
  for (const k of ['n', 'train-k', 'timeout-s', 'mem-gb', 'target-pips']) a[k] = Number(a[k]);
  if (a.parallel !== undefined) a.parallel = Number(a.parallel);
  return a;
}

// ---------------------------------------------------------------------------------------------------- CSV
function csvCell(v) {
  if (v === null || v === undefined || (typeof v === 'number' && !Number.isFinite(v))) return '';
  const s = typeof v === 'number' ? String(+v.toPrecision(10)) : String(v);
  return /[",\n]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s;
}
function writeCsv(file, rows, cols) {
  cols = cols || (rows.length ? Object.keys(rows[0]) : ['row_id']);
  fs.writeFileSync(file, [cols.join(','), ...rows.map(r => cols.map(c => csvCell(r[c])).join(','))].join('\n') + '\n');
}
function parseCsv(text) {
  const rows = []; let row = [], cell = '', q = false;
  for (let i = 0; i < text.length; i++) {
    const ch = text[i];
    if (q) {
      if (ch === '"') { if (text[i + 1] === '"') { cell += '"'; i++; } else q = false; }
      else cell += ch;
    } else if (ch === '"') q = true;
    else if (ch === ',') { row.push(cell); cell = ''; }
    else if (ch === '\n') { row.push(cell); rows.push(row); row = []; cell = ''; }
    else if (ch !== '\r') cell += ch;
  }
  if (cell !== '' || row.length) { row.push(cell); rows.push(row); }
  const [head, ...body] = rows;
  return body.filter(r => r.length === head.length).map(r => Object.fromEntries(head.map((h, i) => [h, r[i]])));
}
const readCsv = f => parseCsv(fs.readFileSync(f, 'utf8'));
const num = v => (v === undefined || v === null || v === '' || v === 'na' || v === 'undefined') ? null : (Number.isFinite(+v) ? +v : null);

// ---------------------------------------------------------------------------------------------------- statistics
const mean = xs => xs.length ? xs.reduce((s, x) => s + x, 0) / xs.length : null;
function sd(xs) { if (xs.length < 2) return null; const m = mean(xs); return Math.sqrt(xs.reduce((s, x) => s + (x - m) ** 2, 0) / (xs.length - 1)); }
function median(xs) { if (!xs.length) return null; const s = [...xs].sort((a, b) => a - b); const h = s.length >> 1; return s.length % 2 ? s[h] : (s[h - 1] + s[h]) / 2; }
function lgamma(x) {
  const c = [76.18009172947146, -86.50532032941677, 24.01409824083091, -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5];
  let y = x, t = x + 5.5; t -= (x + 0.5) * Math.log(t); let s = 1.000000000190015;
  for (const ci of c) s += ci / ++y;
  return -t + Math.log(2.5066282746310005 * s / x);
}
function betacf(a, b, x) {
  const FP = 1e-300; let c = 1, d = 1 - (a + b) * x / (a + 1); if (Math.abs(d) < FP) d = FP; d = 1 / d; let h = d;
  for (let m = 1; m <= 300; m++) {
    const m2 = 2 * m; let aa = m * (b - m) * x / ((a - 1 + m2) * (a + m2));
    d = 1 + aa * d; if (Math.abs(d) < FP) d = FP; c = 1 + aa / c; if (Math.abs(c) < FP) c = FP; d = 1 / d; h *= d * c;
    aa = -(a + m) * (a + b + m) * x / ((a + m2) * (a + 1 + m2));
    d = 1 + aa * d; if (Math.abs(d) < FP) d = FP; c = 1 + aa / c; if (Math.abs(c) < FP) c = FP; d = 1 / d;
    const del = d * c; h *= del; if (Math.abs(del - 1) < 3e-14) break;
  }
  return h;
}
function ibeta(a, b, x) {
  if (x <= 0) return 0; if (x >= 1) return 1;
  const bt = Math.exp(lgamma(a + b) - lgamma(a) - lgamma(b) + a * Math.log(x) + b * Math.log(1 - x));
  return x < (a + 1) / (a + b + 2) ? bt * betacf(a, b, x) / a : 1 - bt * betacf(b, a, 1 - x) / b;
}
const tTwoSidedP = (t, df) => ibeta(df / 2, 0.5, df / (df + t * t));
function tCrit(alpha, df) { let lo = 0, hi = 1e7; for (let i = 0; i < 200; i++) { const m = (lo + hi) / 2; if (tTwoSidedP(m, df) > alpha) lo = m; else hi = m; } return (lo + hi) / 2; }
function summarise(values) {
  const n = values.length, m = mean(values), s = sd(values), se = s === null ? null : s / Math.sqrt(n);
  const t = se ? m / se : null;
  return { n, mean: m, sd: s, se, t, df: n >= 2 ? n - 1 : null, p_two_sided: t === null ? null : tTwoSidedP(Math.abs(t), n - 1) };
}
// Block standard error of money per episode: each block's money is one observation, sqrt(B) * sd(block money) / K
// (resim.erl's formula). Here an episode belongs to the day or ISO week its first bar falls in.
function blockSe(episodes, keyFn) {
  const blocks = new Map();
  for (const e of episodes) { const k = keyFn(e.start); blocks.set(k, (blocks.get(k) || 0) + e.money); }
  const vals = [...blocks.values()];
  if (vals.length < 2 || !episodes.length) return { se: null, blocks: vals.length };
  return { se: Math.sqrt(vals.length) * sd(vals) / episodes.length, blocks: vals.length };
}
function isoWeek(dateStr) {
  const d = new Date(dateStr.slice(0, 10) + 'T00:00:00Z');
  const day = (d.getUTCDay() + 6) % 7; d.setUTCDate(d.getUTCDate() - day + 3);
  const y = d.getUTCFullYear(); const jan4 = new Date(Date.UTC(y, 0, 4));
  return y + '-W' + (1 + Math.round(((d - jan4) / 864e5 - 3 + ((jan4.getUTCDay() + 6) % 7)) / 7));
}
function maxDrawdown(series) { let cum = 0, peak = 0, dd = 0; for (const x of series) { cum += x; peak = Math.max(peak, cum); dd = Math.max(dd, peak - cum); } return dd; }

// ---------------------------------------------------------------------------------------------------- log parsing
function parseSpan(s) {
  const m = /\{\{(\d+),(\d+),(\d+)\},\{(\d+),(\d+),(\d+)\}\}/.exec(s || '');
  if (!m) return null;
  const d = (y, mo, da) => `${y}-${String(mo).padStart(2, '0')}-${String(da).padStart(2, '0')}`;
  return { from: d(m[1], m[2], m[3]), to: d(m[4], m[5], m[6]) };
}
function parseConfig(line) {
  const cfg = {};
  for (const seg of line.replace(/^config:\s*/, '').split(' | '))
    for (const tok of seg.trim().split(/\s+/)) {
      const i = tok.indexOf('='); if (i <= 0) continue;
      const k = tok.slice(0, i), v = tok.slice(i + 1);
      // "val" appears twice: the number of validation episodes, then the validation span.
      if (v.startsWith('{{')) cfg[k + '_span'] = v; else if (!(k in cfg)) cfg[k] = v;
    }
  return cfg;
}
const firstInt = s => { const m = /-?\d+/.exec(s || ''); return m ? +m[0] : null; };

function parseExpRunner(text) {
  const exps = []; let exp = null, run = null;
  const ensureExp = id => { exp = exps.find(e => e.id === id); if (!exp) { exp = { id, runs: new Map() }; exps.push(exp); } };
  for (const line of text.split('\n')) {
    let m;
    if ((m = /\| experiment: <<"([^"]+)">>/.exec(line))) { ensureExp(m[1]); continue; }
    if ((m = /^=== RUN (\d+) ===/.exec(line))) { if (!exp) ensureExp('unknown'); run = { index: +m[1] }; continue; }
    if (run && (m = /\| population: <<"([^"]+)">> \| mode: (\S+)/.exec(line))) { run.population = m[1]; run.mode = m[2]; continue; }
    if (run && line.startsWith('config: ')) {
      run.config = parseConfig(line);
      const prev = exp.runs.get(run.index);
      exp.runs.set(run.index, { ...(prev || {}), ...run, resumed: !!prev });
      continue;
    }
    if ((m = /status: completed \| best_fitness: (\S+) \| avg_fitness: (\S+) \| generations: (\d+)/.exec(line)) && run && exp.runs.get(run.index))
      exp.runs.get(run.index).completed = { generations: +m[3] };
  }
  return exps;
}

function parseGenSummary(text) {
  const out = [];
  for (const line of text.split('\n')) {
    const parts = line.split(' | ');
    if (parts[2] !== 'GEN_SUMMARY') continue;
    const pop = (/<<"([^"]+)">>/.exec(parts[1]) || [])[1];
    const kv = {};
    for (const p of parts.slice(3)) { const i = p.indexOf('='); if (i > 0) kv[p.slice(0, i).trim()] = p.slice(i + 1).trim(); }
    out.push({ time: parts[0], population: pop, kv });
  }
  return out;
}

// ---------------------------------------------------------------------------------------------------- discovery
function discover(results, out) {
  const isRun = d => fs.existsSync(path.join(d, 'logs', 'exp_runner', 'exp_runner.log'));
  const dirs = [];
  if (isRun(results)) dirs.push(results);
  for (const name of fs.readdirSync(results).sort()) {
    const d = path.join(results, name);
    if (name.startsWith('_') || name.startsWith('.') || path.resolve(d) === out || !fs.statSync(d).isDirectory()) continue;
    if (isRun(d)) dirs.push(d);
  }
  return dirs;
}

function loadFolder(dir, a, problems) {
  const folder = path.basename(dir);
  const exps = parseExpRunner(fs.readFileSync(path.join(dir, 'logs', 'exp_runner', 'exp_runner.log'), 'utf8'));
  const gsFile = path.join(dir, 'logs', 'Benchmarker', 'generation_summary.log');
  const gen = fs.existsSync(gsFile) ? parseGenSummary(fs.readFileSync(gsFile, 'utf8')) : [];
  if (!fs.existsSync(gsFile)) problems.push(`${folder}: no generation_summary.log`);
  const backup = path.join(dir, 'mnesia.nonode@nohost.backup');
  const runs = [];
  for (const e of exps) for (const [idx, r] of [...e.runs].sort((x, y) => x[0] - y[0])) {
    const c = r.config || {};
    const arm = c.arm || 'reference';
    const train = parseSpan(c.train_span), val = parseSpan(c.val_span);
    const baseArm = arm.includes('+') ? arm.split('+')[0] : (arm.startsWith('fold_') ? 'rec_level' : arm);
    const fold = arm.includes('fold_') ? arm.slice(arm.indexOf('fold_')) : (val ? 'fold_' + val.from.slice(0, 4) : 'unknown');
    const spread = num(c.spread);
    runs.push({
      folder, dir, experiment: e.id, run: idx, population: r.population, arm, base_arm: baseArm, fold,
      evolution_seed: firstInt(c.evolution_seed), episode_seed: firstInt(c.episode_seed),
      train_from: train && train.from, train_to: train && train.to, val_from: val && val.from, val_to: val && val.to,
      spread, cost_schedule: c.cost_schedule, rollover_rule: c.rollover_rule, late_fill: c.late_fill,
      min_trades: num(c.min_trades), episodes_val: num(c.val), resumed: r.resumed,
      completed_generations: r.completed ? r.completed.generations : null,
      target: spread !== null && Math.abs(spread - a['target-pips'] * 1e-4) < 1e-12 && c.cost_schedule === '[]',
      backup_present: fs.existsSync(backup),
    });
  }
  // Hard rule: no span after 2017.
  const bad = runs.filter(r => !r.train_to || !r.val_to || r.train_to > LAST_DAY || r.val_to > LAST_DAY);
  if (bad.length) return { folder, refused: bad.map(r => `run ${r.run}: train ${r.train_from}..${r.train_to}, validation ${r.val_from}..${r.val_to}`) };
  return { folder, dir, backup, runs, gen };
}

// ---------------------------------------------------------------------------------------------------- learning curves
const LC_COLS = ['best_score', 'median_score', 'survivor_rescore', 'survivors', 'eligible', 'net_pips_ep', 'entries_per_ep',
  'neurons_avg', 'neurons_max', 'gen_seconds', 'ms_per_bar', 'positive', 'gross_pips_trade', 'spread', 'agents', 'stalls', 'runaways', 'vm_mb',
  'rho_score_money', 'median_alpha_money'];
const VAL_COLS = ['val_agents', 'val_positive', 'val_net_pips_ep', 'val_t_all', 'val_t_top', 'val_alpha_all', 'val_alpha_top',
  'rho_score_val', 'val_spread', 'rev60_val', 'rev240_val', 'val_above_rule',
  'flat_val', 'flat_val_pips', 'long_val', 'long_val_pips', 'mom60_val', 'mom60_val_pips', 'clock_val', 'clock_val_pips'];

function curves(F, ids) {
  const byPop = new Map(F.runs.map(r => [r.population, r]));
  const lc = [], val = [];
  // last line per population and generation (a resumed run can repeat a generation)
  const seen = new Map();
  for (const g of F.gen) seen.set(g.population + '#' + g.kv.generation, g);
  const lines = [...seen.values()].filter(g => byPop.has(g.population))
    .sort((x, y) => byPop.get(x.population).run - byPop.get(y.population).run || +x.kv.generation - +y.kv.generation);
  const unmatched = F.gen.filter(g => !byPop.has(g.population)).length;
  let offset = 0, lastRun = null, maxGen = -1;
  for (const g of lines) {
    const r = byPop.get(g.population);
    if (lastRun !== null && r.run !== lastRun) { offset += maxGen + 1; maxGen = -1; }
    lastRun = r.run; const gen = +g.kv.generation; maxGen = Math.max(maxGen, gen);
    const base = { folder: F.folder, arm: r.arm, fold: r.fold, evolution_seed: r.evolution_seed, run: r.run, population: r.population,
      generation: gen, cum_generation: offset + gen };
    const row = { row_id: 'lc-' + ids.lc++, ...base };
    for (const c of LC_COLS) row[c] = num(g.kv[c]);
    row.validated = num(g.kv.val_agents) > 0;
    lc.push(row);
    if (num(g.kv.val_agents) > 0) {
      const v = { row_id: 'val-' + ids.val++, ...base };
      for (const c of VAL_COLS) v[c] = num(g.kv[c]);
      val.push(v);
    }
  }
  return { lc, val, unmatched };
}

function curveSummary(lc, ids) {
  const out = []; const groups = new Map();
  for (const r of lc) { const k = r.folder + '#' + r.run; if (!groups.has(k)) groups.set(k, []); groups.get(k).push(r); }
  for (const rows of groups.values()) {
    const first = rows[0], last = rows[rows.length - 1], win = rows.slice(-LAST_WINDOW);
    const o = { row_id: 'cs-' + ids.cs++, folder: first.folder, arm: first.arm, fold: first.fold, evolution_seed: first.evolution_seed,
      run: first.run, generations: rows.length, first_generation: first.generation, last_generation: last.generation,
      first_row: first.row_id, last_row: last.row_id, window: win.length };
    for (const c of ['median_score', 'best_score', 'survivor_rescore', 'net_pips_ep', 'entries_per_ep', 'neurons_avg', 'gen_seconds']) {
      o['first_' + c] = first[c]; o['last_' + c] = last[c];
      o['window_mean_' + c] = mean(win.map(r => r[c]).filter(x => x !== null));
    }
    out.push(o);
  }
  return out;
}

// ---------------------------------------------------------------------------------------------------- re-simulation
function conditionName(cost, rollover, late) {
  const c = Number.isInteger(cost) ? cost.toFixed(1) : String(cost);
  return `cost${c}_rollover_${rollover ? 'on' : 'off'}_fill_${late ? 'late' : 'now'}`;
}
function trainingCondition(r, a) { return conditionName(a['target-pips'], r.rollover_rule === 'true', r.late_fill === 'true'); }

async function resimFolder(F, a, ctx, log) {
  const jobs = F.runs.filter(r => r.target);
  for (const r of jobs) {
    const label = 'run' + r.run;
    const outDir = path.join(a.out, 'resim', F.folder);
    const jobDir = path.join(outDir, label);
    const params = { population: r.population, n: a.n, train_k: a['train-k'], conditions: a.conditions, backup_mtime: fs.statSync(F.backup).mtimeMs };
    const paramFile = path.join(jobDir, 'params.json');
    if (fs.existsSync(path.join(jobDir, 'done.txt')) && fs.existsSync(paramFile) && fs.readFileSync(paramFile, 'utf8') === JSON.stringify(params)) {
      r.resim = 'done (kept from an earlier pass)'; r.resim_dir = jobDir; continue;
    }
    vm.ensureDir(jobDir);
    fs.rmSync(path.join(jobDir, 'done.txt'), { force: true });
    const dir = vm.prepareVmDir(a.work, `resim_${F.folder}_${label}`, ctx.prices);
    const spec = { backup: F.backup, out_dir: outDir, experiment_id: { bin: r.experiment }, n_best: a.n, train_k: a['train-k'],
      conditions: a.conditions === 'all' ? { atom: 'all' } : a.conditions.split(','),
      jobs: [{ run_index: r.run, population_id: { bin: r.population }, label }] };
    if (a.parallel) spec.parallel = a.parallel;
    const specFile = path.join(dir, 'spec.term');
    fs.writeFileSync(specFile, vm.term(spec) + '.\n');
    const nCond = a.conditions === 'all' ? 12 : a.conditions.split(',').length;
    const estBars = a.n * (a['train-k'] + (r.episodes_val || 400)) * nCond * 1000;
    log(`resim ${F.folder} ${label}: ${a.n} agents x (${a['train-k']} + up to ${r.episodes_val || 400}) episodes x ${nCond} conditions = about ${(estBars / 1e6).toFixed(0)} M bars, ` +
      `about ${Math.round(estBars / 70000 / 60)} min at 70,000 bars/s (cap ${a['timeout-s']} s)`);
    const res = await vm.runVm({ dir, ebin: ctx.ebin, evalExpr: `dxnn_results:resim_job(${JSON.stringify(specFile)})`, env: {},
      timeoutS: a['timeout-s'], memMb: a['mem-gb'] * 1024, logFile: path.join(jobDir, 'vm.log') });
    fs.rmSync(dir, { recursive: true, force: true });
    log(`  -> ${res.ok ? 'ok' : 'FAILED'} in ${res.seconds} s, peak ${res.peakMb} MB${res.killed ? ', killed: ' + res.killed : ''}${res.error ? ', ' + res.error : ''}`);
    if (res.ok && fs.existsSync(path.join(jobDir, 'done.txt'))) { fs.writeFileSync(paramFile, JSON.stringify(params)); r.resim = 'done'; r.resim_dir = jobDir; }
    else r.resim = 'failed: ' + (res.killed || res.error || 'exit ' + res.code) + ' (see ' + path.relative(a.out, path.join(jobDir, 'vm.log')) + ')';
    r.resim_seconds = res.seconds; r.resim_peak_mb = res.peakMb;
  }
}

function collectResim(F, ids, tables, a) {
  for (const r of F.runs.filter(x => x.resim_dir)) {
    const jd = r.resim_dir;
    const chosen = readCsv(path.join(jd, 'chosen.csv'));
    const rank = new Map(chosen.map(c => [c.agent, c]));
    for (const c of chosen) tables.chosen.push({ row_id: 'ch-' + ids.ch++, folder: F.folder, arm: r.arm, fold: r.fold, evolution_seed: r.evolution_seed,
      run: r.run, population: r.population, rank: +c.rank, agent: c.agent, fitness: num(c.fitness), generation: num(c.generation),
      neurons: num(c.neurons), clones_skipped: num(c.clones_skipped_in_population) });
    const base = readCsv(path.join(jd, 'baselines.csv'));
    for (const span of ['train', 'validation']) {
      const dir = path.join(jd, 'Resim', span);
      const agents = readCsv(path.join(dir, 'agents.csv'));
      const eps = readCsv(path.join(dir, 'episodes.csv'));
      const conds = [...new Set(agents.map(x => x.condition))];
      for (const cond of conds) {
        const ag = agents.filter(x => x.condition === cond);
        const agentRows = ag.map(x => {
          const row = { row_id: 'agt-' + ids.agt++, folder: F.folder, arm: r.arm, fold: r.fold, evolution_seed: r.evolution_seed, run: r.run,
            population: r.population, span, condition: cond, rank: rank.has(x.agent) ? +rank.get(x.agent).rank : null, agent: x.agent,
            fitness_stored: rank.has(x.agent) ? num(rank.get(x.agent).fitness) : null, episodes: num(x.episodes),
            money_pips_per_episode: num(x.money_pips_per_episode), se_day: num(x.se_day_blocks), t_day: num(x.t_day),
            se_week: num(x.se_week_blocks), t_week: num(x.t_week), positions_per_episode: num(x.positions_per_episode),
            bars_per_position: num(x.bars_per_position), max_drawdown_pips: num(x.max_drawdown_pips), eligible: x.eligible,
            fitness_resim: num(x.fitness), errors: x.errors };
          tables.perAgent.push(row); return row;
        });
        const money = agentRows.map(x => x.money_pips_per_episode).filter(x => x !== null);
        const elig = agentRows.filter(x => x.eligible === 'true').map(x => x.money_pips_per_episode).filter(x => x !== null);
        // Equal-weight portfolio of the N agents, episode by episode.
        const byEp = new Map();
        for (const e of eps.filter(x => x.condition === cond)) {
          if (!byEp.has(e.episode_start)) byEp.set(e.episode_start, []);
          byEp.get(e.episode_start).push(+e.money_pips);
        }
        const port = [...byEp.entries()].sort((x, y) => x[0] < y[0] ? -1 : 1).map(([start, ms]) => ({ start, money: mean(ms), n: ms.length }));
        const pm = mean(port.map(p => p.money));
        const week = blockSe(port, isoWeek), day = blockSe(port, s => s.slice(0, 10));
        const bl = {}; const blRow = {};
        for (const rule of RULES) {
          const b = base.find(x => x.condition === cond && x.span === span && x.rule === rule);
          bl[rule] = b ? num(b.money_pips_per_episode) : null;
        }
        const strongest = (rules) => rules.filter(k => bl[k] !== null).sort((x, y) => bl[y] - bl[x])[0] || null;
        const s1 = strongest(RULES), s2 = strongest(RULES.filter(k => k !== 'long'));
        for (const rule of RULES) blRow['baseline_' + rule] = bl[rule];
        tables.pops.push({ row_id: 'pop-' + ids.pop++, folder: F.folder, arm: r.arm, base_arm: r.base_arm, fold: r.fold,
          evolution_seed: r.evolution_seed, episode_seed: r.episode_seed, run: r.run, final_population: r.run === Math.max(...F.runs.map(x => x.run)),
          population: r.population, span, condition: cond, training_condition: cond === trainingCondition(r, a),
          agents: agentRows.length, eligible: elig.length, episodes: port.length,
          median_money: median(money), median_money_eligible: median(elig), mean_money: mean(money),
          positive_agents: money.filter(x => x > 0).length, rank1_money: (agentRows.find(x => x.rank === 1) || {}).money_pips_per_episode ?? null,
          portfolio_money: pm, portfolio_se_week: week.se, portfolio_t_week: week.se ? pm / week.se : null, weeks: week.blocks,
          portfolio_se_day: day.se, portfolio_t_day: day.se ? pm / day.se : null, days: day.blocks,
          portfolio_max_drawdown_pips: maxDrawdown(port.map(p => p.money)),
          max_within_episode_drawdown_pips: Math.max(0, ...agentRows.map(x => x.max_drawdown_pips).filter(x => x !== null)),
          rebuild_errors: agentRows.reduce((s, x) => s + (num(x.errors) || 0), 0),
          ...blRow, strongest_baseline: s1, strongest_baseline_money: s1 ? bl[s1] : null,
          strongest_baseline_excl_long: s2, strongest_baseline_excl_long_money: s2 ? bl[s2] : null,
          agent_rows: agentRows.length ? agentRows[0].row_id + '..' + agentRows[agentRows.length - 1].row_id : '' });
      }
    }
  }
}

// ---------------------------------------------------------------------------------------------------- comparison
function outcomesFor(F, tables, a) {
  const lastRun = Math.max(...F.runs.map(r => r.run));
  const final = F.runs.find(r => r.run === lastRun);
  const vals = tables.val.filter(v => v.folder === F.folder && v.run === lastRun);
  const v = vals[vals.length - 1];
  const popRow = cond => tables.pops.find(p => p.folder === F.folder && p.run === lastRun && p.span === 'validation' && p.condition === cond);
  const tr = popRow(trainingCondition(final, a)), ro = popRow(conditionName(a['target-pips'], true, false));
  return [
    { outcome: 'final_population_validation_median', value: v ? v.val_net_pips_ep : null, source: v ? 'validation.csv ' + v.row_id : 'none: no validation reading in the last run' },
    { outcome: 'bestN_validation_median_training_rule', value: tr ? tr.median_money : null, source: tr ? 'populations.csv ' + tr.row_id : 'none: last run not re-simulated' },
    { outcome: 'bestN_validation_median_rollover_rule_0.25', value: ro ? ro.median_money : null, source: ro ? 'populations.csv ' + ro.row_id : 'none: last run not re-simulated under this condition' },
  ];
}

function compare(folders, tables, a, ids) {
  const cells = [];
  for (const F of folders) {
    const r0 = F.runs[0];
    for (const o of outcomesFor(F, tables, a)) cells.push({ ...o, folder: F.folder, arm: r0.base_arm, fold: r0.fold, seed: r0.evolution_seed, episode_seed: r0.episode_seed });
  }
  const rows = []; let comparisons = 0;
  const groups = [...new Set(cells.map(c => c.fold + '|' + c.episode_seed))];
  for (const g of groups) {
    const [fold, epSeed] = g.split('|');
    const signatures = new Map();
    for (const outcome of [...new Set(cells.map(c => c.outcome))]) {
      const cs = cells.filter(c => c.fold + '|' + c.episode_seed === g && c.outcome === outcome);
      // An outcome whose every cell is the same CSV row as an earlier outcome's is the same measurement: listed, not
      // compared again, so it does not inflate the count of comparisons.
      const sig = cs.map(c => c.folder + c.source).sort().join(';');
      if (signatures.has(sig) && cs.some(c => c.value !== null)) {
        rows.push({ row_id: 'cmp-' + ids.cmp++, fold, episode_seed: epSeed === 'null' ? null : +epSeed, outcome, kind: 'same_as', term: signatures.get(sig),
          source: 'every value is the same CSV row as outcome ' + signatures.get(sig) + ' (same condition); not compared again' });
        continue;
      }
      signatures.set(sig, outcome);
      const arms = [...ARM_ORDER.filter(x => cs.some(c => c.arm === x)), ...[...new Set(cs.map(c => c.arm))].filter(x => !ARM_ORDER.includes(x))];
      const val = (arm, seed) => { const c = cs.find(x => x.arm === arm && x.seed === seed && x.value !== null); return c ? c.value : null; };
      const seeds = [...new Set(cs.map(c => c.seed))].sort((x, y) => x - y);
      const common = { fold, episode_seed: epSeed === 'null' ? null : +epSeed, outcome };
      for (const c of cs) rows.push({ row_id: 'cmp-' + ids.cmp++, ...common, kind: 'cell', term: c.arm, seeds: String(c.seed), n: c.value === null ? 0 : 1, mean: c.value, source: c.folder + ': ' + c.source });
      for (const arm of arms) {
        const xs = seeds.map(s => val(arm, s)).filter(x => x !== null);
        rows.push({ row_id: 'cmp-' + ids.cmp++, ...common, kind: 'arm_mean', term: arm, seeds: seeds.filter(s => val(arm, s) !== null).join(' '), ...summarise(xs), t: null, p_two_sided: null });
      }
      const counted = x => { if (x.n >= 2) comparisons++; return x; };
      for (let i = 0; i < arms.length; i++) for (let j = i + 1; j < arms.length; j++) {
        const ss = seeds.filter(s => val(arms[i], s) !== null && val(arms[j], s) !== null);
        rows.push(counted({ row_id: 'cmp-' + ids.cmp++, ...common, kind: 'paired_difference', term: `${arms[j]} minus ${arms[i]}`, seeds: ss.join(' '),
          ...summarise(ss.map(s => val(arms[j], s) - val(arms[i], s))) }));
      }
      const full = seeds.filter(s => ARM_ORDER.every(arm => val(arm, s) !== null));
      const effects = [
        ['recurrent minus feedforward', s => (val('rec_level', s) + val('rec_vol', s)) / 2 - (val('ff_level', s) + val('ff_vol', s)) / 2],
        ['vol minus level', s => (val('ff_vol', s) + val('rec_vol', s)) / 2 - (val('ff_level', s) + val('rec_level', s)) / 2],
        ['interaction (rec_vol - ff_vol) - (rec_level - ff_level)', s => (val('rec_vol', s) - val('ff_vol', s)) - (val('rec_level', s) - val('ff_level', s))],
      ];
      for (const [name, f] of effects)
        rows.push(counted({ row_id: 'cmp-' + ids.cmp++, ...common, kind: 'effect', term: name, seeds: full.join(' '), ...summarise(full.map(f)),
          ...(full.length ? {} : { source: 'needs all four arms for a seed' }) }));
    }
  }
  return { rows, comparisons };
}

function foldRows(folders, tables, a, ids) {
  const rows = [];
  for (const F of folders) {
    const lastRun = Math.max(...F.runs.map(r => r.run));
    const r = F.runs.find(x => x.run === lastRun);
    const p = tables.pops.find(x => x.folder === F.folder && x.run === lastRun && x.span === 'validation' && x.condition === trainingCondition(r, a));
    rows.push({ row_id: 'fold-' + ids.fold++, kind: 'run', fold: r.fold, arm: r.base_arm, evolution_seed: r.evolution_seed, folder: F.folder,
      validation_year: r.val_from ? r.val_from.slice(0, 4) : null, condition: trainingCondition(r, a), population: r.population,
      agents: p ? p.agents : null, episodes: p ? p.episodes : null,
      money_pips_per_episode: p ? p.portfolio_money : null, se_week: p ? p.portfolio_se_week : null, se_day: p ? p.portfolio_se_day : null,
      t_week: p ? p.portfolio_t_week : null, median_agent_money: p ? p.median_money : null,
      strongest_baseline: p ? p.strongest_baseline : null, strongest_baseline_money: p ? p.strongest_baseline_money : null,
      strongest_baseline_excl_long: p ? p.strongest_baseline_excl_long : null, strongest_baseline_excl_long_money: p ? p.strongest_baseline_excl_long_money : null,
      long_money: p ? p.baseline_long : null, largest_drawdown_pips: p ? p.portfolio_max_drawdown_pips : null,
      largest_within_episode_drawdown_pips: p ? p.max_within_episode_drawdown_pips : null, source: p ? 'populations.csv ' + p.row_id : 'final population not re-simulated' });
  }
  const groups = new Map();
  for (const r of rows) { const k = r.fold + '|' + r.arm; if (!groups.has(k)) groups.set(k, []); groups.get(k).push(r); }
  for (const rs of groups.values()) if (rs.length > 1) {
    const ok = rs.filter(r => r.money_pips_per_episode !== null);
    const s = summarise(ok.map(r => r.money_pips_per_episode));
    const same = k => new Set(ok.map(r => r[k])).size === 1 ? ok[0][k] : 'differs by seed';
    rows.push({ row_id: 'fold-' + ids.fold++, kind: 'mean over seeds', fold: rs[0].fold, arm: rs[0].arm, evolution_seed: ok.map(r => r.evolution_seed).join(' '),
      validation_year: rs[0].validation_year, condition: rs[0].condition, money_pips_per_episode: s.mean, se_week: s.se,
      median_agent_money: mean(ok.map(r => r.median_agent_money).filter(x => x !== null)),
      strongest_baseline: ok.length ? same('strongest_baseline') : null,
      strongest_baseline_money: mean(ok.map(r => r.strongest_baseline_money).filter(x => x !== null)),
      strongest_baseline_excl_long: ok.length ? same('strongest_baseline_excl_long') : null,
      strongest_baseline_excl_long_money: mean(ok.map(r => r.strongest_baseline_excl_long_money).filter(x => x !== null)),
      long_money: mean(ok.map(r => r.long_money).filter(x => x !== null)),
      largest_drawdown_pips: Math.max(0, ...ok.map(r => r.largest_drawdown_pips).filter(x => x !== null)),
      source: 'mean of ' + ok.map(r => r.row_id).join(' ') + '; se_week here is the standard error across seeds' });
  }
  return rows;
}

// ---------------------------------------------------------------------------------------------------- summary
const f2 = (x, d = 2) => x === null || x === undefined || !Number.isFinite(+x) ? 'n/a' : (+x).toFixed(d);
function mdTable(head, rows) { return ['| ' + head.join(' | ') + ' |', '|' + head.map(() => '---').join('|') + '|', ...rows.map(r => '| ' + r.join(' | ') + ' |')].join('\n'); }

function summary(a, ctx, folders, refused, problems, tables, cmp, folds) {
  const L = [];
  L.push(`# Result-folder analysis: ${path.basename(a.results)}`, '');
  L.push(`Input \`${a.results}\`. Output \`${a.out}\`. Best N = ${a.n}; training-span re-simulation on up to ${a['train-k']} episodes tiled over each run's training span; ` +
    `validation re-simulation on the run's own validation tiling (its \`episodes_val\`); conditions: ${a.conditions}. ` +
    `Money is pips per 1,000-bar episode after cost. Every number below is rounded from the CSV row named next to it (file and \`row_id\`).`, '');
  L.push(`Price file loaded by the VMs: \`${ctx.prices}\` (rows before 2018-01-01 only). No span after ${LAST_DAY} was evaluated.`, '');
  if (refused.length) { L.push('## Refused folders', ''); for (const r of refused) L.push(`- **${r.folder}**: span ends after ${LAST_DAY}: ${r.refused.join('; ')}`); L.push(''); }
  L.push('## Runs', '');
  L.push(mdTable(['folder', 'arm', 'fold', 'evo seed', 'run', 'cost (pips)', 'saved at target cost', 're-simulation', 'runs.csv'],
    tables.runs.map(r => [r.folder, r.arm, r.fold, r.evolution_seed, r.run, f2(r.spread * 1e4), r.target ? 'yes' : 'no', r.resim || '-', r.row_id])), '');
  L.push('## Learning curves', '', `Per run: the last generation, and the mean of the last ${LAST_WINDOW} generations (or all, if fewer). Full curves: \`learning_curve.csv\`.`, '');
  L.push(mdTable(['folder', 'run', 'gens', 'median score (last / window)', 'best score (last)', 'survivors on new episodes (last / window)', 'net pips per episode (last / window)', 'positions per episode (last)', 'neurons (last)', 's per gen (window)', 'curve_summary.csv'],
    tables.cs.map(c => [c.folder, c.run, c.generations, `${f2(c.last_median_score)} / ${f2(c.window_mean_median_score)}`, f2(c.last_best_score),
      `${f2(c.last_survivor_rescore)} / ${f2(c.window_mean_survivor_rescore)}`, `${f2(c.last_net_pips_ep)} / ${f2(c.window_mean_net_pips_ep)}`,
      f2(c.last_entries_per_ep, 1), f2(c.last_neurons_avg, 1), f2(c.window_mean_gen_seconds, 1), c.row_id])), '');
  L.push('## Validation readings (simulator\'s own, whole population)', '',
    'Median agent (eligible on score and validation episodes) next to the baselines of the same line. rev60/rev240 are scores in the log; their money comes from the re-simulation of the same episodes (`populations.csv`).', '');
  L.push(mdTable(['folder', 'run', 'gen', 'agents', 'above 0', 'median money', 'flat', 'long', 'mom60', 'clock', 'rev60 / rev240 money', 'validation.csv'],
    tables.val.map(v => [v.folder, v.run, v.generation, v.val_agents, v.val_positive, f2(v.val_net_pips_ep), f2(v.flat_val_pips), f2(v.long_val_pips),
      f2(v.mom60_val_pips), f2(v.clock_val_pips), v.rev60_val_pips_resim === undefined ? 'n/a' : `${f2(v.rev60_val_pips_resim)} / ${f2(v.rev240_val_pips_resim)} (${v.resim_baseline_source})`, v.row_id])), '');
  const trainingPops = tables.pops.filter(p => p.training_condition);
  L.push('## Re-simulation of the best agents, under the training rule', '',
    'Median over the chosen agents, and the equal-weight portfolio of them with week- and day-block standard errors (an episode belongs to the week or day its first bar is in). All twelve conditions: `populations.csv`, per agent: `per_agent.csv`.', '');
  L.push(mdTable(['folder', 'run', 'span', 'agents', 'median money', 'portfolio (se week, se day)', 'strongest baseline', 'strongest excl. long', 'populations.csv'],
    trainingPops.map(p => [p.folder, p.run, p.span, p.agents, f2(p.median_money), `${f2(p.portfolio_money)} (${f2(p.portfolio_se_week)}, ${f2(p.portfolio_se_day)})`,
      `${p.strongest_baseline} ${f2(p.strongest_baseline_money)}`, `${p.strongest_baseline_excl_long} ${f2(p.strongest_baseline_excl_long_money)}`, p.row_id])), '');
  L.push('## Comparison across arms', '');
  L.push(`Outcomes: (1) the final population's validation money, median agent, from the simulator's own last validation reading; (2) the same for the best N, re-simulated under the rule the run trained with; (3) the best N under the rollover rule at 0.25 pip, fill now. ` +
    `For runs trained with the rollover rule on and the fill now (all four arms), outcomes (2) and (3) are the same condition and the same number.`, '');
  const df = [...new Set(cmp.rows.filter(r => r.df).map(r => r.df))];
  const crit = df.map(d => `df ${d}: |t| > ${f2(tCrit(0.05, d))} for p < 0.05 per comparison, |t| > ${f2(tCrit(0.05 / Math.max(1, cmp.comparisons), d))} after Bonferroni over ${cmp.comparisons}`).join('; ');
  L.push(`**Comparisons made: ${cmp.comparisons}** (paired differences and effects with at least two seeds, over the distinct outcomes). Standard errors come from the spread across seeds, with seeds - 1 degrees of freedom; ${crit || 'no comparison had two seeds'}.`, '');
  for (const g of [...new Set(cmp.rows.map(r => r.fold + '|' + r.episode_seed + '|' + r.outcome))]) {
    const rs = cmp.rows.filter(r => r.fold + '|' + r.episode_seed + '|' + r.outcome === g);
    L.push(`### ${rs[0].outcome} (${rs[0].fold}, episode seed ${rs[0].episode_seed})`, '');
    if (rs.length === 1 && rs[0].kind === 'same_as') { L.push(`Same values as ${rs[0].term} (${rs[0].row_id}): not compared again.`, ''); continue; }
    L.push(mdTable(['kind', 'term', 'seeds', 'n', 'mean', 'se', 't', 'comparison.csv'],
      rs.map(r => [r.kind === 'cell' ? 'value' : r.kind, r.term, r.seeds, r.n, f2(r.mean), f2(r.se), f2(r.t), r.row_id])), '');
  }
  L.push('## Folds', '', 'Validation year, final population, best N as an equal-weight portfolio under the training rule. Largest drawdown: of the cumulative money over the year\'s episodes in order.', '');
  L.push(mdTable(['fold', 'arm', 'seed', 'year', 'money (se week)', 'strongest baseline', 'strongest excl. long', 'largest drawdown (pips)', 'folds.csv'],
    folds.map(r => [r.fold, r.arm, r.evolution_seed, r.validation_year, `${f2(r.money_pips_per_episode)} (${f2(r.se_week)})`,
      `${r.strongest_baseline || ''} ${f2(r.strongest_baseline_money)}`, `${r.strongest_baseline_excl_long || ''} ${f2(r.strongest_baseline_excl_long_money)}`, f2(r.largest_drawdown_pips, 1), r.row_id])), '');
  L.push('## Problems and limits found in this pass', '');
  for (const p of problems) L.push('- ' + p);
  if (!problems.length) L.push('- none');
  L.push('');
  return L.join('\n');
}

// ---------------------------------------------------------------------------------------------------- main
(async () => {
  const a = parseArgs(process.argv.slice(2));
  vm.ensureDir(a.out); vm.ensureDir(a.work);
  const logLines = [];
  const log = s => { console.log(s); logLines.push(s); };
  const problems = [];
  const folderDirs = discover(a.results, a.out);
  if (!folderDirs.length) throw new Error('no result folders (logs/exp_runner/exp_runner.log) under ' + a.results);
  const loaded = folderDirs.map(d => loadFolder(d, a, problems));
  const refused = loaded.filter(F => F.refused);
  for (const r of refused) log(`ERROR: ${r.folder} refused: a span ends after ${LAST_DAY}: ${r.refused.join('; ')}`);
  const folders = loaded.filter(F => !F.refused);
  const ids = { run: 1, lc: 1, val: 1, cs: 1, ch: 1, agt: 1, pop: 1, cmp: 1, fold: 1 };
  const tables = { runs: [], lc: [], val: [], cs: [], chosen: [], perAgent: [], pops: [] };

  const ctx = {};
  if (a.resim) { ctx.ebin = vm.ensureBeams(a.work); ctx.prices = await vm.ensureCutPrices(a.work); } else ctx.prices = '(no VM started: --no-resim)';
  for (const F of folders) {
    if (F.runs.length === 0) { problems.push(`${F.folder}: no run in exp_runner.log`); continue; }
    if (new Set(F.runs.map(r => r.experiment)).size > 1) problems.push(`${F.folder}: several experiments in one folder; each run is kept, but the comparison uses the arm of the first run`);
    if (new Set(F.runs.map(r => r.arm)).size > 1) problems.push(`${F.folder}: runs of more than one arm in one folder`);
    const c = curves(F, ids);
    tables.lc.push(...c.lc); tables.val.push(...c.val);
    if (c.unmatched) problems.push(`${F.folder}: ${c.unmatched} GEN_SUMMARY lines name a population not in exp_runner.log (not used)`);
    if (a.resim) {
      if (!F.runs[0].backup_present) { problems.push(`${F.folder}: no mnesia.nonode@nohost.backup, not re-simulated`); }
      else await resimFolder(F, a, ctx, log);
    }
    for (const r of F.runs) if (r.target && !r.resim) r.resim = a.resim ? 'not run' : 'skipped (--no-resim)';
    collectResim(F, ids, tables, a);
  }
  for (const F of folders) for (const r of F.runs) {
    tables.runs.push({ row_id: 'run-' + ids.run++, ...Object.fromEntries(Object.entries(r).filter(([k]) => !['dir', 'resim_dir'].includes(k))) });
    if (r.resumed) problems.push(`${F.folder} run ${r.run}: started more than once (resumed); the last GEN_SUMMARY line per generation is used`);
    if (r.resim && r.resim.startsWith('failed')) problems.push(`${F.folder} run ${r.run}: re-simulation ${r.resim}`);
  }
  tables.cs = curveSummary(tables.lc, ids);
  // The reversal rules' money on the validation episodes, from the re-simulation (same episodes, training rule).
  for (const v of tables.val) {
    const F = folders.find(x => x.folder === v.folder); const r = F.runs.find(x => x.run === v.run);
    // Every run of an experiment validates on the same tiling of the same span, so any re-simulated population of the
    // folder has the same validation episodes.
    const q = tables.pops.find(x => x.folder === v.folder && x.span === 'validation' && x.condition === trainingCondition(r, a));
    if (q) { v.rev60_val_pips_resim = q.baseline_rev60; v.rev240_val_pips_resim = q.baseline_rev240; v.resim_baseline_source = 'populations.csv ' + q.row_id; }
  }
  const cmp = compare(folders, tables, a, ids);
  const folds = foldRows(folders, tables, a, ids);
  const rebuildErrors = tables.pops.reduce((s, p) => s + (p.rebuild_errors || 0), 0);
  if (rebuildErrors) problems.push(`${rebuildErrors} agent-condition rows where the money rebuilt from positions differs from the simulator's (per_agent.csv errors)`);

  writeCsv(path.join(a.out, 'runs.csv'), tables.runs);
  writeCsv(path.join(a.out, 'learning_curve.csv'), tables.lc);
  writeCsv(path.join(a.out, 'curve_summary.csv'), tables.cs);
  const valCols = ['row_id', 'folder', 'arm', 'fold', 'evolution_seed', 'run', 'population', 'generation', 'cum_generation', ...VAL_COLS, 'rev60_val_pips_resim', 'rev240_val_pips_resim', 'resim_baseline_source'];
  writeCsv(path.join(a.out, 'validation.csv'), tables.val, valCols);
  writeCsv(path.join(a.out, 'chosen.csv'), tables.chosen);
  writeCsv(path.join(a.out, 'per_agent.csv'), tables.perAgent);
  writeCsv(path.join(a.out, 'populations.csv'), tables.pops);
  writeCsv(path.join(a.out, 'comparison.csv'), cmp.rows, ['row_id', 'fold', 'episode_seed', 'outcome', 'kind', 'term', 'seeds', 'n', 'mean', 'sd', 'se', 't', 'df', 'p_two_sided', 'source']);
  writeCsv(path.join(a.out, 'folds.csv'), folds, ['row_id', 'kind', 'fold', 'arm', 'evolution_seed', 'folder', 'validation_year', 'condition', 'population', 'agents', 'episodes',
    'money_pips_per_episode', 'se_week', 'se_day', 't_week', 'median_agent_money', 'strongest_baseline', 'strongest_baseline_money', 'strongest_baseline_excl_long',
    'strongest_baseline_excl_long_money', 'long_money', 'largest_drawdown_pips', 'largest_within_episode_drawdown_pips', 'source']);
  fs.writeFileSync(path.join(a.out, 'summary.md'), summary(a, ctx, folders, refused, problems, tables, cmp, folds));
  fs.writeFileSync(path.join(a.out, 'pipeline.log'), logLines.join('\n') + '\n');
  log(`wrote ${a.out}/summary.md (${folders.length} folders, ${tables.lc.length} generation lines, ${tables.pops.length} population rows, ${cmp.comparisons} comparisons)`);
  if (refused.length) process.exit(3);
  if (tables.runs.some(r => r.resim && r.resim.startsWith('failed'))) process.exit(4);
})().catch(e => { console.error('ERROR: ' + (e.stack || e.message)); process.exit(1); });
