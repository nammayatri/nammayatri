'use strict';
// maps-shim/ride-start.js -- the ride's start code, for the auth guard (day 6,
// 2026-11-15) -- against a real Postgres (PGlite) holding the driver server's
// ride table, and the fake « whose token is this ».
//
//   cd Backend/dev/local-stack/tests && npm ci && node ride-start.test.js
//
//   1. the code: only for the driver's OWN ride, and only before it starts
//   2. what is refused: no token, a forged one, a bad id, no database
//   3. the route in server.js: POST only, and answered (no database here: 503)
//
// The guard's half -- the code put into « start », the request forwarded as
// sent when there is none -- is in auth-guard-routes.test.js (its golden file).
const http = require('http');
const path = require('path');
const { spawn } = require('child_process');
const { open } = require('./lib/db');
const fakes = require('./lib/fakes');

const SHIM = path.join(__dirname, '..', 'stack', 'maps-shim');
const { startCode } = require(path.join(SHIM, 'ride-start.js'));
const PORT = 8094;

const RIDE = {
  mine: '11111111-1111-4111-8111-111111111111',
  started: '22222222-2222-4222-8222-222222222222',
  others: '33333333-3333-4333-8333-333333333333',
  none: '44444444-4444-4444-8444-444444444444',
};
const D1 = 'd1d1d1d1-0000-4000-8000-000000000001';
const D2 = 'd2d2d2d2-0000-4000-8000-000000000002';

function post(p, body) {
  return new Promise((resolve) => {
    const data = body === undefined ? undefined : JSON.stringify(body);
    const req = http.request({ host: '127.0.0.1', port: PORT, path: p, method: data ? 'POST' : 'GET',
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
  const who = await fakes.backends();
  who.drivers.set('tok-d1', D1);
  who.drivers.set('tok-d2', D2);

  // The live column, which the shared fixture leaves out: otp character(4) NOT NULL.
  await db.pool.query(`ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN otp character(4)`);
  const ride = (id, driver, status, otp) => db.pool.query(
    `INSERT INTO atlas_driver_offer_bpp.ride (id, driver_id, status, otp) VALUES ($1, $2, $3, $4)`,
    [id, driver, status, otp]);
  await ride(RIDE.mine, D1, 'NEW', '4821');
  await ride(RIDE.started, D1, 'INPROGRESS', '1111');
  await ride(RIDE.others, D2, 'NEW', '2222');

  const ask = (token, rideId, pool = db.pool) => startCode(pool, who.url, token, rideId);

  t.section('1. the code: his own ride, not started');
  let r = await ask('tok-d1', RIDE.mine);
  t.ok(r.status === 200 && r.body.code === '4821', 'his ride, NEW: 200 and its code', JSON.stringify(r));
  r = await ask('tok-d1', RIDE.others);
  t.ok(r.status === 404 && r.body.error === 'no_such_ride' && !('code' in r.body),
    "another driver's ride, NEW and real: 404, as if it did not exist -- and no code", JSON.stringify(r));
  r = await ask('tok-d2', RIDE.others);
  t.ok(r.status === 200 && r.body.code === '2222', 'and that driver gets his own', JSON.stringify(r));
  r = await ask('tok-d1', RIDE.started);
  t.ok(r.status === 404 && !('code' in r.body), 'his ride, already started: 404, no code', JSON.stringify(r));
  r = await ask('tok-d1', RIDE.none);
  t.ok(r.status === 404, 'no such ride: 404', JSON.stringify(r));

  t.section('2. what is refused');
  r = await ask('', RIDE.mine);
  t.ok(r.status === 401 && !('code' in r.body), 'no token: 401', JSON.stringify(r));
  r = await ask('tok-forged', RIDE.mine);
  t.ok(r.status === 401 && !('code' in r.body), 'a forged token: 401', JSON.stringify(r));
  r = await ask(undefined, RIDE.mine);
  t.ok(r.status === 401, 'a token that is not a string: 401', JSON.stringify(r));
  for (const [label, id] of [['not an id', "x' OR '1'='1"], ['nothing', ''], ['a number', 42], ['too long', `${RIDE.mine}0`]]) {
    r = await ask('tok-d1', id);
    t.ok(r.status === 400 && r.body.error === 'bad_ride', `a ride id that is ${label}: 400 bad_ride, before any lookup`, JSON.stringify(r));
  }
  r = await ask('tok-d1', RIDE.mine, null);
  t.ok(r.status === 503, 'no database: 503', JSON.stringify(r));

  t.section('3. the route in server.js');
  const shim = spawn(process.execPath, [path.join(SHIM, 'server.js')], {
    env: { PATH: process.env.PATH, PORT: String(PORT), DRIVER_URL: who.url, RIDER_URL: who.url },
    stdio: 'ignore',
  });
  let up = { status: 0 };
  for (let i = 0; i < 50 && up.status === 0; i += 1) {
    await new Promise((res) => setTimeout(res, 100));
    up = await post('/healthz');
  }
  r = await post('/internal/ride-start-code');
  t.ok(r.status === 405, 'a GET: 405', JSON.stringify(r));
  r = await post('/internal/ride-start-code', { token: 'tok-d1', rideId: RIDE.mine });
  t.ok(r.status === 503 && !(r.json && 'code' in r.json), 'a POST reaches ride-start.js (no database in this shim: 503)', JSON.stringify(r));
  shim.kill();

  who.close();
  t.finish();
})().catch((e) => { console.error(e); process.exit(1); });
