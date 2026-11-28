'use strict';
/**
 * /internal/virtual-rider -- the console's « Passager virtuel » button (day 8
 * of « Lieux proposés », 2026-11-18). The engine is virtual-rider.js; this is
 * only how the office starts it, follows it and stops it without ssh.
 *
 *   POST /internal/virtual-rider  {action: 'start', driverId, scenario, from?, to?}
 *     202 {country, startedAt}           the run has begun
 *     400 {error: 'bad_request'}         not a driver id, not a scenario 1-8,
 *                                        a from / to that is not {lat, lon} here
 *     404 {error: 'no_such_driver'}      no driver with that id
 *     409 {error: 'busy', country}       a run is going for that country
 *     503 {error: 'off' | 'unavailable'} VIRTUAL_RIDER is not on / no database
 *   POST {action: 'stop', country}       202 {stopping: true} | 404 {error: 'none'}
 *   POST {action: 'status'}              200 {enabled, runs: {DZ, MR}}
 *
 * Loopback and the docker bridge only, like /internal/driver-push: admin-api
 * calls it from its container; the edge routes nothing under /internal/.
 *
 * ── Why the engine runs as a process of its own (decision V8) ───────────────
 * maps-shim serves the live app's maps, search and wallet. A fault in a test
 * tool must never be able to stop those, so the engine is a child process --
 * exactly the command the owner runs by hand (`--driver <id> <scenario>`),
 * its stdout read line by line. « Arrêter » sends it SIGTERM, as the command
 * line's `stop` does, and it closes its own booking. A restart of maps-shim
 * kills a running child; the next run cancels what it left open
 * (virtual-rider.js, closeLeftovers).
 *
 * The trip (2026-11-19, the boss: « Can we choose depart and end locations? »):
 * `from` / `to`, each optional, passed as `--from lat,lon` / `--to lat,lon`.
 * The engine decides the real points (snapped to a road, or its own when left
 * out) and says them on one machine line, `@trajet {json}`, which becomes the
 * run's `trip` -- for the console's map -- and never one of its lines.
 *
 * One run at a time per country, whoever started it: a run started here, or a
 * command-line run (its pid file) both answer 409. The runs are kept in memory
 * only: after a restart, the status is empty.
 */
const { spawn: realSpawn } = require('child_process');
const fs = require('fs');
const path = require('path');
const engine = require('./virtual-rider');

const ENGINE = path.join(__dirname, 'virtual-rider.js');
const DRIVER_ID = /^[0-9a-f-]{36}$/i;
const MAX_LINES = 300;
const COUNTRIES = ['DZ', 'MR'];

/** Loopback, or a container on the docker bridge (172.16.0.0/12). */
function local(remoteAddress) {
  const from = String(remoteAddress || '').replace(/^::ffff:/, '');
  return from === '127.0.0.1' || from === '::1' || /^172\.(1[6-9]|2\d|3[01])\./.test(from);
}

/** A run started from the command line, still alive? Its pid file says. */
function commandLineRun(country) {
  try {
    const pid = Number(fs.readFileSync(engine.pidFile(country), 'utf8'));
    return pid && engine.alive(pid) ? pid : null;
  } catch {
    return null;
  }
}

/**
 * deps: { pool, spawn?, enabled?, now? } -- the tests give their own spawn.
 */
function create(deps = {}) {
  const pool = deps.pool || null;
  const spawn = deps.spawn || realSpawn;
  const enabled = deps.enabled || (() => process.env.VIRTUAL_RIDER === 'on');
  const now = deps.now || (() => new Date());
  const runs = new Map();

  const add = (run, text) => {
    const line = String(text).replace(/\r$/, '');
    if (!line.trim()) return;
    if (line.startsWith('@trajet ')) {
      try {
        const t = JSON.parse(line.slice(8));
        const p = (x) => engine.point(x);
        if (p(t.driver) && p(t.from) && p(t.to)) {
          run.trip = { driver: p(t.driver), from: p(t.from), to: p(t.to),
            toName: String(t.toName || '').slice(0, 80), chosen: Boolean(t.chosen) };
        }
      } catch { /* not ours: dropped */ }
      return;
    }
    let m;
    if ((m = line.match(/^RÉUSSI : (.*)$/))) run.verdict = { ok: true, reason: m[1] };
    else if ((m = line.match(/^PAS RÉUSSI : (.*)$/))) run.verdict = { ok: false, reason: m[1] };
    else if ((m = line.match(/^ARRÊTÉ : (.*)$/))) run.verdict = { ok: false, stopped: true, reason: m[1] };
    if (run.lines.length < MAX_LINES) run.lines.push(line);
  };

  const running = (country) => {
    const run = runs.get(country);
    return Boolean((run && !run.endedAt) || commandLineRun(country));
  };

  async function start(driverId, scenarioRaw, trip = {}) {
    if (!enabled()) return { status: 503, body: { error: 'off' } };
    const scenario = Number(scenarioRaw);
    if (typeof driverId !== 'string' || !DRIVER_ID.test(driverId) || !engine.SCENARIOS[scenario]) {
      return { status: 400, body: { error: 'bad_request' } };
    }
    const from = trip.from == null ? null : engine.point(trip.from);
    const to = trip.to == null ? null : engine.point(trip.to);
    if ((trip.from != null && !from) || (trip.to != null && !to)) return { status: 400, body: { error: 'bad_request' } };
    if (!pool) return { status: 503, body: { error: 'unavailable' } };
    const country = await engine.countryOfDriver(pool, driverId);
    if (!country) return { status: 404, body: { error: 'no_such_driver' } };
    if (running(country)) return { status: 409, body: { error: 'busy', country } };

    const args = [ENGINE, '--driver', driverId, String(scenario)];
    if (from) args.push('--from', `${from.lat},${from.lon}`);
    if (to) args.push('--to', `${to.lat},${to.lon}`);
    const child = spawn(process.execPath, args, {
      cwd: __dirname,
      env: process.env,
      stdio: ['ignore', 'pipe', 'pipe'],
    });
    const run = {
      country, driverId, scenario, startedAt: now().toISOString(), endedAt: null,
      lines: [], verdict: null, child, trip: null, asked: { from, to },
    };
    runs.set(country, run);

    let pending = '';
    child.stdout.setEncoding('utf8');
    child.stdout.on('data', (chunk) => {
      pending += chunk;
      let i;
      while ((i = pending.indexOf('\n')) >= 0) {
        add(run, pending.slice(0, i));
        pending = pending.slice(i + 1);
      }
    });
    child.stderr.setEncoding('utf8');
    child.stderr.on('data', (chunk) => {
      for (const l of String(chunk).split('\n')) if (l.trim()) add(run, `[erreur] ${l}`);
    });
    const finish = (code) => {
      if (run.endedAt) return;
      if (pending) add(run, pending);
      pending = '';
      run.endedAt = now().toISOString();
      run.child = null;
      if (!run.verdict) {
        run.verdict = { ok: false, reason: `le moteur s’est arrêté sans verdict (code ${code})` };
      }
    };
    child.on('exit', finish);
    child.on('error', (e) => { add(run, `[erreur] ${e.message}`); finish('spawn'); });
    return { status: 202, body: { country, startedAt: run.startedAt } };
  }

  function stop(country) {
    if (!COUNTRIES.includes(country)) return { status: 400, body: { error: 'bad_request' } };
    const run = runs.get(country);
    if (run && run.child && !run.endedAt) {
      run.child.kill('SIGTERM');
      return { status: 202, body: { stopping: true } };
    }
    const pid = commandLineRun(country);
    if (pid) {
      process.kill(pid, 'SIGTERM');
      return { status: 202, body: { stopping: true, fromCommandLine: true } };
    }
    return { status: 404, body: { error: 'none' } };
  }

  function status() {
    const view = {};
    for (const c of COUNTRIES) {
      const run = runs.get(c);
      if (run) {
        view[c] = {
          driverId: run.driverId, scenario: run.scenario, startedAt: run.startedAt,
          endedAt: run.endedAt, running: !run.endedAt, lines: run.lines, verdict: run.verdict,
          trip: run.trip, asked: run.asked,
        };
      } else if (commandLineRun(c)) {
        view[c] = { running: true, fromCommandLine: true, lines: [], verdict: null };
      } else {
        view[c] = null;
      }
    }
    return { status: 200, body: { enabled: enabled(), scenarios: engine.SCENARIOS, runs: view } };
  }

  /** The whole route, for server.js. */
  function handle(req, res, send) {
    if (!local(req.socket.remoteAddress)) return send(res, 403, { error: 'local callers only' });
    if (req.method !== 'POST') return send(res, 405, { error: 'method not allowed' });
    const chunks = [];
    let size = 0;
    req.on('data', (c) => {
      size += c.length;
      if (size > 1024) return void req.destroy();
      chunks.push(c);
    });
    req.on('end', async () => {
      let body = {};
      try { body = JSON.parse(Buffer.concat(chunks).toString('utf8')); } catch { /* checked below */ }
      let r;
      try {
        if (body.action === 'start') r = await start(body.driverId, body.scenario, { from: body.from, to: body.to });
        else if (body.action === 'stop') r = stop(body.country);
        else if (body.action === 'status') r = status();
        else r = { status: 400, body: { error: 'bad_request' } };
      } catch (e) {
        console.error(`[virtual-rider] ${e.message}`);
        r = { status: 503, body: { error: 'unavailable' } };
      }
      send(res, r.status, r.body);
    });
    return undefined;
  }

  return { start, stop, status, handle, runs };
}

module.exports = { create, local };
