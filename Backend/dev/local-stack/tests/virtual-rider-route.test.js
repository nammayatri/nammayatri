'use strict';
// maps-shim/virtual-rider-route.js -- /internal/virtual-rider, the console's
// « Passager virtuel » button (day 8, 2026-11-18) -- against PGlite (whose
// country a driver is) and a fake child process; then the REAL engine as the
// child, then server.js itself.
//
//   cd Backend/dev/local-stack/tests && npm ci && node virtual-rider-route.test.js
//
//   1. start: refused when off, malformed, unknown; one run per country (409),
//      whether the other run was started here or from the command line
//   2. what a run says: its lines, its verdict, a stop, a child that dies
//   3. the real engine as the child: the process chain end to end
//   4. server.js: the route answers, POST only; the local-callers guard
const fs = require('fs');
const http = require('http');
const os = require('os');
const path = require('path');
const { EventEmitter } = require('events');
const { PassThrough } = require('stream');
const { spawn } = require('child_process');
const { open } = require('./lib/db');
const fakes = require('./lib/fakes');

const PID_DIR = fs.mkdtempSync(path.join(os.tmpdir(), 'vr-route-'));
process.env.VIRTUAL_RIDER_PID_DIR = PID_DIR;   // before the engine is loaded
const SHIM = path.join(__dirname, '..', 'stack', 'maps-shim');
const { create, local } = require(path.join(SHIM, 'virtual-rider-route.js'));

const DZ_DRIVER = 'd2d2d2d2-0000-4000-8000-000000000001';
const MR_DRIVER = 'a1a1a1a1-0000-4000-8000-000000000002';
const NOBODY = 'eeeeeeee-0000-4000-8000-000000000009';
const PORT = 8097;

/** A child process that does what the test tells it to. */
function fakeChild() {
  const c = new EventEmitter();
  c.stdout = new PassThrough();
  c.stderr = new PassThrough();
  c.killed = [];
  c.kill = (sig) => {
    c.killed.push(sig);
    // What the engine does on SIGTERM: closes its booking, says so, exits 130.
    c.stdout.write('[  12 s] Arrêt demandé : le passager ferme ce qu’il a ouvert.\nARRÊTÉ : rien n’est resté ouvert.\n');
    setImmediate(() => c.emit('exit', 130));
  };
  return c;
}
const tick = () => new Promise((r) => setTimeout(r, 20));

function post(p, body, method = 'POST') {
  return new Promise((resolve) => {
    const data = body === undefined ? undefined : JSON.stringify(body);
    const req = http.request({ host: '127.0.0.1', port: PORT, path: p, method,
      headers: data ? { 'content-type': 'application/json' } : {} }, (res) => {
      let b = '';
      res.on('data', (c) => { b += c; });
      res.on('end', () => { let j = null; try { j = JSON.parse(b); } catch { /* text */ } resolve({ status: res.statusCode, json: j }); });
    });
    req.on('error', () => resolve({ status: 0, json: null }));
    if (data) req.write(data);
    req.end();
  });
}

(async () => {
  const t = fakes.checker();
  const db = await open();
  await db.sql(`INSERT INTO atlas_driver_offer_bpp.person (id, merchant_id, role) VALUES
                ($1, 'algeria0-0000-0000-0000-00000algeria', 'DRIVER'),
                ($2, 'favorit0-0000-0000-0000-00000favorit', 'DRIVER')`, [DZ_DRIVER, MR_DRIVER]);

  const spawned = [];
  let nextChild = null;
  const route = (opts = {}) => create({
    pool: db.pool,
    enabled: () => opts.enabled ?? true,
    spawn: (cmd, args, o) => { const c = nextChild || fakeChild(); nextChild = null; spawned.push({ cmd, args, o, c }); return c; },
  });

  t.section('1. start');
  let r = route({ enabled: false });
  let a = await r.start(DZ_DRIVER, 1);
  t.ok(a.status === 503 && a.body.error === 'off' && spawned.length === 0, 'switched off: 503 « off », nothing started', JSON.stringify(a));
  r = route();
  for (const [label, id, sc] of [['not an id', "x' OR 1=1", 1], ['no id', undefined, 1], ['scenario 0', DZ_DRIVER, 0], ['scenario 9', DZ_DRIVER, 9], ['scenario « 1; rm »', DZ_DRIVER, '1; rm']]) {
    a = await r.start(id, sc);
    t.ok(a.status === 400 && spawned.length === 0, `${label}: 400, before any lookup`, JSON.stringify(a));
  }
  a = await r.start(NOBODY, 1);
  t.ok(a.status === 404 && a.body.error === 'no_such_driver' && spawned.length === 0, 'an id no driver has: 404');
  a = await create({ pool: null, enabled: () => true, spawn: () => fakeChild() }).start(DZ_DRIVER, 1);
  t.ok(a.status === 503 && a.body.error === 'unavailable', 'no database: 503 « unavailable »');

  a = await r.start(DZ_DRIVER, 2);
  const first = spawned.at(-1);
  t.ok(a.status === 202 && a.body.country === 'DZ', 'a driver of Algeria: 202, country DZ', JSON.stringify(a));
  t.ok(first.cmd === process.execPath && first.args[0].endsWith('virtual-rider.js')
    && first.args.slice(1).join(' ') === `--driver ${DZ_DRIVER} 2`, 'the engine is run as its own process: --driver <id> <scenario>', first.args.join(' '));
  t.ok(first.o.stdio[0] === 'ignore', '   with nothing on its stdin');
  a = await r.start(DZ_DRIVER, 1);
  t.ok(a.status === 409 && a.body.country === 'DZ' && spawned.length === 1, 'a second run for Algeria: 409, not started');
  a = await r.start(MR_DRIVER, 1);
  t.ok(a.status === 202 && a.body.country === 'MR' && spawned.length === 2, 'Mauritania meanwhile: 202, the countries are independent');

  t.section('2. what a run says');
  first.c.stdout.write('[   0 s] Scénario 2 : le passager annule avant l’arrivée du chauffeur.\n[   4 s] Demande envoy');
  await tick();
  let s = r.status().body;
  t.ok(s.enabled === true && s.runs.DZ.running && s.runs.DZ.lines.length === 1 && s.runs.DZ.scenario === 2,
    'status: the run, its lines so far (a half line waits for its end)', JSON.stringify(s.runs.DZ.lines));
  t.ok(s.scenarios[1] && s.scenarios[8], '   and the scenarios, for the console to name them');
  first.c.stdout.write('ée (211).\n');
  first.c.stderr.write('a warning\n');
  await tick();
  s = r.status().body;
  t.ok(s.runs.DZ.lines[1] === '[   4 s] Demande envoyée (211).' && s.runs.DZ.lines[2] === '[erreur] a warning', 'the rest of the line, and stderr marked « [erreur] »', JSON.stringify(s.runs.DZ.lines));

  a = r.stop('DZ');
  await tick();
  s = r.status().body;
  t.ok(a.status === 202 && first.c.killed.join() === 'SIGTERM', '« Arrêter »: SIGTERM, what the command line\'s stop sends');
  t.ok(!s.runs.DZ.running && s.runs.DZ.verdict.stopped && /rien n’est resté ouvert/.test(s.runs.DZ.verdict.reason) && s.runs.DZ.endedAt,
    '   the run ends, verdict « arrêté », what the engine said', JSON.stringify(s.runs.DZ.verdict));
  a = r.stop('DZ');
  t.ok(a.status === 404, 'stopping again: 404 « none »');
  t.ok(r.stop('XX').status === 400, 'a country that is not ours: 400');

  const mr = spawned[1];
  mr.c.stdout.write('[ 147 s] Course terminée, prix 211.\nRÉUSSI : course COMPLETED, prix 211 ; le passager a donné 5 étoiles, le chauffeur 4\n');
  mr.c.emit('exit', 0);
  await tick();
  s = r.status().body;
  t.ok(s.runs.MR.verdict.ok === true && /COMPLETED/.test(s.runs.MR.verdict.reason) && !s.runs.MR.running, 'a RÉUSSI line becomes the verdict');

  a = await r.start(DZ_DRIVER, 1);
  const third = spawned.at(-1);
  third.c.stdout.write('PAS RÉUSSI : Karim n’est pas en ligne\n');
  third.c.emit('exit', 1);
  await tick();
  t.ok(a.status === 202 && r.status().body.runs.DZ.verdict.ok === false, 'once ended, a new run may start; PAS RÉUSSI is the verdict');
  a = await r.start(DZ_DRIVER, 1);
  spawned.at(-1).c.emit('exit', 1);
  await tick();
  t.ok(/sans verdict/.test(r.status().body.runs.DZ.verdict.reason), 'a child that dies without a verdict: said so');

  // A run from the command line holds the country too (its pid file).
  fs.writeFileSync(path.join(PID_DIR, 'virtual-rider-DZ.pid'), String(process.pid));
  a = await r.start(DZ_DRIVER, 1);
  t.ok(a.status === 409, 'a command-line run alive for Algeria: 409 from the console too');
  t.ok(r.status().body.runs.DZ.verdict !== undefined, '   (and status still shows the last console run)');
  fs.unlinkSync(path.join(PID_DIR, 'virtual-rider-DZ.pid'));

  // The line cap.
  a = await r.start(DZ_DRIVER, 1);
  const chatty = spawned.at(-1);
  chatty.c.stdout.write(Array.from({ length: 400 }, (_, i) => `ligne ${i}`).join('\n') + '\n');
  await tick();
  t.ok(r.status().body.runs.DZ.lines.length === 300, 'at most 300 lines kept per run');
  chatty.c.emit('exit', 0);

  // A trip chosen on the console's map (2026-11-19).
  a = await r.start(DZ_DRIVER, 1, { from: { lat: 36.76, lon: 3.06 }, to: { lat: 36.79, lon: 3.07 } });
  const chosen = spawned.at(-1);
  t.ok(a.status === 202 && chosen.args.slice(1).join(' ') === `--driver ${DZ_DRIVER} 1 --from 36.76,3.06 --to 36.79,3.07`,
    'a chosen trip: passed as --from lat,lon --to lat,lon', chosen.args.slice(1).join(' '));
  chosen.c.stdout.write('@trajet {"driver":{"lat":36.75,"lon":3.05},"from":{"lat":36.7601,"lon":3.0602},"to":{"lat":36.79,"lon":3.07},"toName":"Arrivée choisie","chosen":true}\n[   0 s] Départ choisi, à 1100 m de lui.\n');
  await tick();
  s = r.status().body.runs.DZ;
  t.ok(s.trip && s.trip.from.lat === 36.7601 && s.trip.driver.lon === 3.05 && s.trip.chosen === true && s.asked.from.lat === 36.76,
    '   the engine\'s @trajet line becomes the run\'s trip, for the map; what was asked is kept too', JSON.stringify(s.trip));
  t.ok(s.lines.length === 1 && !s.lines.some((l) => l.startsWith('@trajet')), '   and never one of its lines', JSON.stringify(s.lines));
  chosen.c.stdout.write('@trajet {"driver":{"lat":99,"lon":3},"from":{"lat":36.7,"lon":3},"to":{"lat":36.8,"lon":3}}\n@trajet pas du json\n');
  await tick();
  t.ok(r.status().body.runs.DZ.trip.from.lat === 36.7601 && r.status().body.runs.DZ.lines.length === 1,
    '   a trip outside our countries, or not JSON: dropped, the last good one kept');
  chosen.c.emit('exit', 0);
  await tick();
  for (const [label, trip] of [['a start in Paris', { from: { lat: 48.85, lon: 2.35 } }], ['an arrival « here »', { to: 'here' }],
    ['an arrival with no lon', { to: { lat: 36.7 } }]]) {
    const before = spawned.length;
    a = await r.start(DZ_DRIVER, 1, trip);
    t.ok(a.status === 400 && spawned.length === before, `${label}: 400, nothing started`, JSON.stringify(a));
  }

  t.section('3. the real engine as the child');
  // No PG_URL in its environment: it starts, refuses, says why, and the
  // route reads that as the verdict -- the whole process chain, for real.
  const savedPg = process.env.PG_URL;
  delete process.env.PG_URL;
  process.env.VIRTUAL_RIDER = 'on';
  const real = create({ pool: db.pool, enabled: () => true });
  a = await real.start(DZ_DRIVER, 1);
  let v = null;
  for (let i = 0; i < 100 && !v; i += 1) { await new Promise((res) => setTimeout(res, 100)); v = real.status().body.runs.DZ.verdict; }
  t.ok(a.status === 202 && v && v.ok === false && /pas de base de données/.test(v.reason),
    'spawned for real: it runs, refuses without a database, and that is the verdict', JSON.stringify(v));
  t.ok(!fs.existsSync(path.join(PID_DIR, 'virtual-rider-DZ.pid')), '   and its pid file is gone');
  if (savedPg !== undefined) process.env.PG_URL = savedPg;

  t.section('4. server.js');
  const shim = spawn(process.execPath, [path.join(SHIM, 'server.js')], {
    env: { PATH: process.env.PATH, PORT: String(PORT), VIRTUAL_RIDER: 'on', VIRTUAL_RIDER_PID_DIR: PID_DIR },
    stdio: 'ignore',
  });
  let up = { status: 0 };
  for (let i = 0; i < 50 && up.status === 0; i += 1) { await new Promise((res) => setTimeout(res, 100)); up = await post('/healthz', undefined, 'GET'); }
  let h = await post('/internal/virtual-rider', undefined, 'GET');
  t.ok(h.status === 405, 'a GET: 405');
  h = await post('/internal/virtual-rider', { action: 'status' });
  t.ok(h.status === 200 && h.json.enabled === true && h.json.runs.DZ === null && h.json.runs.MR === null, 'status: 200, nothing running', JSON.stringify(h.json));
  h = await post('/internal/virtual-rider', { action: 'start', driverId: DZ_DRIVER, scenario: 1 });
  t.ok(h.status === 503 && h.json.error === 'unavailable', 'start without a database in this shim: 503');
  h = await post('/internal/virtual-rider', { action: 'start', driverId: DZ_DRIVER, scenario: 1, from: { lat: 'x', lon: 1 } });
  t.ok(h.status === 400, 'a start whose « from » is not a point: 400 through server.js too');
  h = await post('/internal/virtual-rider', { action: 'drop tables' });
  t.ok(h.status === 400, 'an unknown action: 400');
  shim.kill();

  t.ok(local('127.0.0.1') && local('::ffff:127.0.0.1') && local('172.17.0.2') && local('172.31.255.1'), 'local callers: loopback and the docker bridge');
  t.ok(!local('172.32.0.1') && !local('10.0.0.5') && !local('41.200.1.1') && !local(''), 'anything else is refused (403)');

  await db.close();
  t.finish();
})().catch((e) => { console.error(e); process.exit(1); });
