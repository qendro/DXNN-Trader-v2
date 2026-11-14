// Shared helpers for analysis/pipeline.js and analysis/make_test_run.js: compile the Erlang sources into a work
// directory, cut the price file before 2018, and run one Erlang VM at a time under a memory and a time cap.
'use strict';
const fs = require('node:fs');
const path = require('node:path');
const readline = require('node:readline');
const { spawn, spawnSync, execFileSync } = require('node:child_process');

const REPO = path.resolve(__dirname, '..');
// The only price file this tooling opens. EURUSD_M1_MIDPOINT_UTC.txt (2025 onward) is never named here.
const PRICE_SOURCE = path.join(REPO, 'fx_tables', 'EURUSD1.txt');
const CUT_YEAR = '2018'; // rows from this year on are never copied

function ensureDir(d) { fs.mkdirSync(d, { recursive: true }); return d; }

// ensureBeams: every repo .erl and analysis/dxnn_results.erl compiled into <work>/ebin when a source is newer.
function ensureBeams(work) {
  const ebin = ensureDir(path.join(work, 'ebin'));
  const sources = fs.readdirSync(REPO).filter(f => f.endsWith('.erl')).map(f => path.join(REPO, f));
  sources.push(path.join(__dirname, 'dxnn_results.erl'));
  const stale = sources.filter(src => {
    const beam = path.join(ebin, path.basename(src, '.erl') + '.beam');
    return !fs.existsSync(beam) || fs.statSync(beam).mtimeMs < fs.statSync(src).mtimeMs;
  });
  if (stale.length) {
    const r = spawnSync('erlc', ['-o', ebin, '-I', REPO, ...stale], { encoding: 'utf8' });
    if (r.status !== 0) throw new Error('erlc failed:\n' + r.stdout + r.stderr);
  }
  return ebin;
}

// ensureCutPrices: <work>/prices/EURUSD1.txt holds the rows of fx_tables/EURUSD1.txt before 2018-01-01. Reading stops at
// the first row of 2018; a row out of order before that is an error. The result is checked to end in 2017 or earlier.
async function ensureCutPrices(work) {
  if (path.basename(PRICE_SOURCE) !== 'EURUSD1.txt') throw new Error('price source must be EURUSD1.txt');
  const dir = ensureDir(path.join(work, 'prices'));
  const dest = path.join(dir, 'EURUSD1.txt');
  if (fs.existsSync(dest)) { checkCutFile(dest); return dest; }
  const tmp = dest + '.part';
  const out = fs.createWriteStream(tmp);
  const input = fs.createReadStream(PRICE_SOURCE, { encoding: 'utf8' });
  const rl = readline.createInterface({ input, crlfDelay: Infinity });
  let prev = '', rows = 0;
  for await (const line of rl) {
    if (!line) continue;
    const stamp = line.slice(0, 19);
    if (line.slice(0, 4) >= CUT_YEAR) break;
    if (stamp < prev) { rl.close(); input.destroy(); out.destroy(); throw new Error('price file out of order at ' + stamp); }
    prev = stamp;
    if (!out.write(line + '\n')) await new Promise(r => out.once('drain', r));
    rows++;
  }
  rl.close(); input.destroy();
  await new Promise((res, rej) => out.end(err => err ? rej(err) : res()));
  fs.renameSync(tmp, dest);
  checkCutFile(dest);
  console.log(`prices: ${rows} rows up to ${prev} -> ${dest}`);
  return dest;
}

function checkCutFile(file) {
  const size = fs.statSync(file).size;
  const fd = fs.openSync(file, 'r');
  const buf = Buffer.alloc(Math.min(4096, size));
  fs.readSync(fd, buf, 0, buf.length, size - buf.length);
  fs.closeSync(fd);
  const lines = buf.toString('utf8').trim().split('\n');
  const last = lines[lines.length - 1];
  if (!(last.slice(0, 4) < CUT_YEAR)) throw new Error(`cut price file ends in ${last.slice(0, 10)}, not before ${CUT_YEAR}`);
}

// prepareVmDir: a fresh working directory for one VM: fx_tables/ with the cut prices, an empty Mnesia directory.
function prepareVmDir(work, name, cutPrices) {
  const dir = path.join(work, 'vm', name);
  fs.rmSync(dir, { recursive: true, force: true });
  ensureDir(path.join(dir, 'fx_tables'));
  fs.symlinkSync(cutPrices, path.join(dir, 'fx_tables', 'EURUSD1.txt'));
  fs.writeFileSync(path.join(dir, 'fx_tables', 'EURUSD1_LIVE.txt'), '');
  ensureDir(path.join(dir, 'Mnesia'));
  return dir;
}

// Erlang term text for a JS value: strings become strings, {bin: s} binaries, {atom: s} atoms, objects maps.
function term(v) {
  if (v === null || v === undefined) return 'undefined';
  if (typeof v === 'number') return Number.isInteger(v) ? String(v) : v.toExponential(10).replace('e', 'e');
  if (typeof v === 'boolean') return String(v);
  if (typeof v === 'string') return JSON.stringify(v);
  if (Array.isArray(v)) return '[' + v.map(term).join(',') + ']';
  if (v.bin !== undefined) return '<<' + JSON.stringify(v.bin) + '>>';
  if (v.atom !== undefined) return v.atom;
  if (v.tuple !== undefined) return '{' + v.tuple.map(term).join(',') + '}';
  return '#{' + Object.entries(v).map(([k, x]) => `${k}=>${term(x)}`).join(',') + '}';
}

function treeRssMb(rootPid) {
  let out;
  try { out = execFileSync('ps', ['-A', '-o', 'pid=,ppid=,rss='], { encoding: 'utf8' }); } catch { return 0; }
  const rows = out.trim().split('\n').map(l => l.trim().split(/\s+/).map(Number));
  const kids = new Map();
  for (const [pid, ppid] of rows) { if (!kids.has(ppid)) kids.set(ppid, []); kids.get(ppid).push(pid); }
  const rss = new Map(rows.map(([pid, , r]) => [pid, r]));
  let total = 0; const stack = [rootPid];
  while (stack.length) { const p = stack.pop(); total += rss.get(p) || 0; for (const k of kids.get(p) || []) stack.push(k); }
  return total / 1024;
}

// runVm: one VM, erl +Bd +scl false, killed when its process tree passes memMb or it runs longer than timeoutS.
// Resolves {code, killed, peakMb, seconds, ok}. ok is true when the VM printed DXNN_RESULTS_OK and exited 0.
function runVm({ dir, ebin, evalExpr, env, timeoutS, memMb, logFile }) {
  return new Promise(resolve => {
    const args = ['+Bd', '+scl', 'false', '-noshell', '-pa', ebin, '-mnesia', 'dir', JSON.stringify(path.join(dir, 'Mnesia')), '-eval', evalExpr];
    const log = fs.openSync(logFile, 'w');
    fs.writeSync(log, `# erl ${args.join(' ')}\n# cwd ${dir}\n`);
    const child = spawn('erl', args, { cwd: dir, env: { ...process.env, ...env }, stdio: ['ignore', log, log] });
    const t0 = Date.now();
    let peak = 0, killed = null;
    const timer = setInterval(() => {
      const mb = treeRssMb(child.pid);
      peak = Math.max(peak, mb);
      if (mb > memMb) { killed = `memory ${mb.toFixed(0)} MB > ${memMb} MB`; child.kill('SIGKILL'); }
      else if ((Date.now() - t0) / 1000 > timeoutS) { killed = `time > ${timeoutS} s`; child.kill('SIGKILL'); }
    }, 2000);
    child.on('exit', code => {
      clearInterval(timer);
      fs.closeSync(log);
      const text = fs.readFileSync(logFile, 'utf8');
      const ok = code === 0 && !killed && text.includes('DXNN_RESULTS_OK');
      const err = (text.match(/DXNN_RESULTS_ERROR[^\n]*/) || [null])[0];
      resolve({ code, killed, peakMb: Math.round(peak), seconds: Math.round((Date.now() - t0) / 1000), ok, error: err });
    });
  });
}

module.exports = { REPO, ensureDir, ensureBeams, ensureCutPrices, prepareVmDir, runVm, term };
