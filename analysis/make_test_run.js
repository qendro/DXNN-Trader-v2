#!/usr/bin/env node
// A small evolution run whose folder looks like a cloud result folder (mnesia.nonode@nohost.backup, run.out, logs/).
// For testing analysis/pipeline.js only.
//
//   node analysis/make_test_run.js --out <folder> --arm rec_vol --evolution-seed 1 [--episode-seed 1]
//        [--agents 24] [--score 8] [--val 40] [--runs 1:1,3:1,4:1] [--work <dir>] [--timeout-s 540] [--mem-gb 3.5]
//
// --runs takes entries of the arm's run list (exp_runner:get_run_configs/0) as SourceRun:Generations, renumbered from 1.
// The default keeps the free run and two quarter-pip runs, one generation limit each.
'use strict';
const fs = require('node:fs');
const path = require('node:path');
const vm = require('./vm');

function args(argv) {
  const a = { 'episode-seed': '1', agents: '24', score: '8', val: '40', runs: '1:1,3:1,4:1', 'timeout-s': '540', 'mem-gb': '3.5' };
  for (let i = 0; i < argv.length; i++) if (argv[i].startsWith('--')) a[argv[i].slice(2)] = argv[++i];
  for (const k of ['out', 'arm', 'evolution-seed']) if (!a[k]) throw new Error('missing --' + k);
  a.work = a.work || path.join(path.dirname(path.resolve(a.out)), '_work');
  return a;
}

(async () => {
  const a = args(process.argv.slice(2));
  const out = vm.ensureDir(path.resolve(a.out));
  if (fs.existsSync(path.join(out, 'mnesia.nonode@nohost.backup'))) throw new Error(out + ' already holds a backup');
  const work = vm.ensureDir(path.resolve(a.work));
  const ebin = vm.ensureBeams(work);
  const prices = await vm.ensureCutPrices(work);
  const name = 'test_' + path.basename(out);
  const dir = vm.prepareVmDir(work, name, prices);
  const runs = a.runs.split(',').map(s => { const [r, g] = s.split(':').map(Number); return { tuple: [r, g] }; });
  const spec = { backup: path.join(out, 'mnesia.nonode@nohost.backup'), agents: +a.agents, episodes_score: +a.score,
    episodes_val: +a.val, runs, timeout_s: Math.max(30, +a['timeout-s'] - 30) };
  const specFile = path.join(dir, 'spec.term');
  fs.writeFileSync(specFile, vm.term(spec) + '.\n');
  const env = { DXNN_TEST_ARM: a.arm, DXNN_EPISODE_SEED: a['episode-seed'], DXNN_EVOLUTION_SEED: a['evolution-seed'],
    QLOG_ROOT: path.join(out, 'logs') };
  console.log(`test run ${a.arm} evolution seed ${a['evolution-seed']} -> ${out}`);
  const r = await vm.runVm({ dir, ebin, evalExpr: `dxnn_results:test_run(${JSON.stringify(specFile)})`, env,
    timeoutS: +a['timeout-s'], memMb: +a['mem-gb'] * 1024, logFile: path.join(out, 'run.out') });
  console.log(JSON.stringify(r));
  fs.rmSync(dir, { recursive: true, force: true });
  if (!r.ok) process.exit(1);
})().catch(e => { console.error(e.message); process.exit(1); });
