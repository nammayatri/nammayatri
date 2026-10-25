'use strict';
// maps-shim, route by route, against a golden file. Phase 5 step 4 (2026-10-06):
// the safety net for splitting server.js into one module per subject with NO
// behaviour change.
//
//   node tests/maps-shim-routes.test.js            compare with the golden file
//   node tests/maps-shim-routes.test.js --record   (re)write it -- only from code
//                                                  whose behaviour is the reference
//
// The real server.js runs as a process. Around it: a fake OSRM and a fake
// mock-google that record what they are asked, and a scripted `pg` (see
// lib/fake-pg-preload.js) that records every query against the place index.
// For each request the golden file holds the status, the body, the SQL and
// params sent, and the OSRM / mock-google URLs asked. A split that changes any
// one of those -- an answer, a query, a URL -- fails here.
const fs = require('fs');
const http = require('http');
const os = require('os');
const path = require('path');
const { spawn } = require('child_process');

const SHIM = path.join(__dirname, '..', 'stack', 'maps-shim', 'server.js');
const PRELOAD = path.join(__dirname, 'lib', 'fake-pg-preload.js');
const GOLDEN = path.join(__dirname, 'fixtures', 'maps-shim-routes.golden.json');
const PORT = 8093;
const RECORD = process.argv.includes('--record');

// Two OSRM steps: real Google-encoded polylines (precision 5).
const STEP_A = '_p~iF~ps|U_ulLnnqC';
const STEP_B = '_mqNvxq`@';
function osrmAnswer(url) {
  if (url.includes('-15.9,18.2')) return { status: 200, body: { code: 'NoRoute', routes: [] } };
  if (url.includes('-15.8,18.3')) return { status: 200, raw: 'not json' };
  const leg = (d) => ({
    distance: d, duration: d / 8,
    steps: [
      { distance: d * 0.6, duration: d / 12, geometry: STEP_A, maneuver: { location: [-15.97, 18.08] } },
      { distance: d * 0.4, duration: d / 24, geometry: STEP_B, maneuver: { location: [-15.96, 18.09] } },
      { distance: 0, duration: 0, geometry: '', maneuver: { location: [-15.95, 18.10] } },
    ],
  });
  const routes = [{ distance: 4321.5, duration: 610.2, legs: [leg(4321.5)] }];
  if (url.includes('alternatives=true')) routes.push({ distance: 76543, duration: 4000, legs: [leg(5000), leg(71543)] });
  return { status: 200, body: { code: 'Ok', routes } };
}

const REQUESTS = [
  // directions
  'GET /directions/json?origin=18.0858,-15.9785&destination=18.1,-15.95&key=k&mode=driving',
  'GET /directions/json?origin=18.0858,-15.9785&destination=18.1,-15.95&alternatives=true&waypoints=via:18.09,-15.97|18.095,-15.96',
  `GET /directions/json?origin=18.0858,-15.9785&destination=18.1,-15.95&waypoints=${Array.from({ length: 11 }, (_, i) => `18.0${i},-15.9${i}`).join('|')}`,
  'GET /directions/json?origin=abc&destination=18.1,-15.95',
  'GET /directions/json?origin=999,1&destination=18.1,-15.95',
  'GET /directions/json?origin=18.0858,-15.9785',
  'GET /directions/json?origin=18.0858,-15.9785&destination=18.2,-15.9',
  'GET /directions/json?origin=18.0858,-15.9785&destination=18.3,-15.8',
  // search
  'GET /place/autocomplete/json?input=ma&location=18.0858,-15.9785&radius=5000&key=k',
  'GET /place/autocomplete/json?input=marche',
  'GET /place/autocomplete/json?input=x&location=18.0858,-15.9785',
  'GET /place/autocomplete/json?input=zzz&location=18.0858,-15.9785',
  'GET /place/autocomplete/json?input=boom&location=18.0858,-15.9785',
  `GET /place/autocomplete/json?input=${'a'.repeat(150)}&location=1e308,5`,
  'GET /place/details/json?place_id=osm:n1&fields=geometry',
  'GET /place/details/json?place_id=osm:w2',
  'GET /place/details/json?place_id=osm:none',
  'GET /place/details/json',
  'GET /place/labels/json?ids=osm:n1,osm:w2,osm:n3&lang=ar',
  'GET /place/labels/json?ids=osm:n1&lang=fr',
  'GET /place/labels/json?ids=',
  `GET /place/labels/json?ids=${Array.from({ length: 30 }, (_, i) => `osm:x${i}`).join(',')}`,
  'GET /geocode/json?latlng=18.0866,-15.9750',
  'GET /geocode/json?latlng=36.7650,3.0500',
  'GET /geocode/json?latlng=18.0,-30.5',
  'GET /geocode/json?place_id=osm:n3',
  'GET /geocode/json?latlng=999,0',
  'GET /geocode/json',
  // the router around them
  'GET /healthz',
  'GET /wallet/status',
  'GET /some/other/google/path?x=1',
  'POST /directions/json?origin=18.0858,-15.9785&destination=18.1,-15.95',
];

const listen = (handler) => new Promise((r) => { const s = http.createServer(handler); s.listen(0, '127.0.0.1', () => r(s)); });

function call(line) {
  const [method, p] = line.split(' ');
  return new Promise((resolve) => {
    const req = http.request({ host: '127.0.0.1', port: PORT, path: p, method }, (res) => {
      let t = '';
      res.setEncoding('utf8');
      res.on('data', (c) => { t += c; });
      res.on('end', () => resolve({ status: res.statusCode, type: res.headers['content-type'], text: t }));
    });
    req.on('error', (e) => resolve({ status: 0, text: String(e) }));
    req.end();
  });
}

(async () => {
  const asked = [];
  const osrm = await listen((req, res) => {
    asked.push(`osrm ${req.url}`);
    const a = osrmAnswer(req.url);
    res.writeHead(a.status, { 'content-type': 'application/json' });
    res.end(a.raw !== undefined ? a.raw : JSON.stringify(a.body));
  });
  const google = await listen((req, res) => {
    asked.push(`mock-google ${req.method} ${req.url}`);
    res.writeHead(200, { 'content-type': 'application/json' });
    res.end('{"status":"OK","from":"mock-google"}');
  });
  const osrmUrl = `http://127.0.0.1:${osrm.address().port}`;
  const googleUrl = `http://127.0.0.1:${google.address().port}`;
  const pgLog = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'shim-')), 'pg.jsonl');
  fs.writeFileSync(pgLog, '');

  const shim = spawn(process.execPath, ['-r', PRELOAD, SHIM], {
    cwd: path.dirname(SHIM),
    env: {
      PATH: process.env.PATH, PORT: String(PORT), OSRM_URL: osrmUrl, MOCK_GOOGLE_URL: googleUrl,
      PG_URL: 'postgres://fake', FAKE_PG_LOG: pgLog,
      // Nothing else reachable: Redis, the backends and the gateways are all
      // pointed at a closed port, so no timer can wander off the machine.
      REDIS_HOST: '127.0.0.1', REDIS_PORT: '9', DRIVER_URL: 'http://127.0.0.1:9', RIDER_URL: 'http://127.0.0.1:9',
      WALLET_SWEEP_MS: '3600000', RESTRICTED_REFRESH_MS: '3600000',
    },
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  let log = '';
  shim.stdout.on('data', (c) => { log += c; });
  shim.stderr.on('data', (c) => { log += c; });
  for (let i = 0; i < 60; i += 1) {
    if ((await call('GET /healthz')).status === 200) break;
    await new Promise((r) => setTimeout(r, 100));
  }

  const norm = (s) => String(s).split(osrmUrl).join('<osrm>').split(googleUrl).join('<mock-google>');
  const results = [];
  for (const line of REQUESTS) {
    asked.length = 0;
    const before = fs.readFileSync(pgLog, 'utf8').length;
    const r = await call(line);
    await new Promise((ok) => setTimeout(ok, 20));
    const queries = fs.readFileSync(pgLog, 'utf8').slice(before).split('\n').filter(Boolean).map((l) => JSON.parse(l));
    let body;
    try { body = JSON.parse(norm(r.text)); } catch { body = norm(r.text); }
    results.push({ request: line, status: r.status, type: r.type, body, queries, asked: asked.map(norm) });
  }
  shim.kill();
  osrm.close();
  google.close();

  if (RECORD) {
    fs.mkdirSync(path.dirname(GOLDEN), { recursive: true });
    fs.writeFileSync(GOLDEN, JSON.stringify(results, null, 1) + '\n');
    console.log(`recorded ${results.length} requests to ${path.relative(process.cwd(), GOLDEN)}`);
    process.exit(0);
  }

  const golden = JSON.parse(fs.readFileSync(GOLDEN, 'utf8'));
  let failed = 0;
  for (let i = 0; i < REQUESTS.length; i += 1) {
    const want = JSON.stringify(golden[i]);
    const got = JSON.stringify(results[i]);
    const same = want === got;
    console.log(`${same ? '  ok  ' : '  BAD '} ${REQUESTS[i].slice(0, 100)}`);
    if (!same) {
      failed += 1;
      console.log(`         want ${want.slice(0, 400)}\n         got  ${got.slice(0, 400)}`);
    }
  }
  if (golden.length !== REQUESTS.length) { failed += 1; console.log('  BAD  the golden file has a different number of requests'); }
  if (failed) console.log('\n--- shim log ---\n' + log.slice(-1500));
  console.log(failed ? `\nFAILED (${failed})` : `\nALL PASSED -- ${REQUESTS.length} requests answered exactly as recorded`);
  process.exit(failed ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(2); });
