'use strict';
// Account deletion requests: maps-shim/deletion.js against a real Postgres
// (PGlite) and our own migration, db/account-deletion.sql. Phase 5 (2026-10-06).
//
//   cd Backend/dev/local-stack/tests && npm ci && node deletion.test.js
//
// Nothing in that file deletes anything: it records a request the office
// carries out within 30 days. What must hold: the id always comes from the
// token, one open request per account, never while a ride is under way, and
// a check that fails refuses rather than allows.
const path = require('path');
const { open } = require('./lib/db');
const fakes = require('./lib/fakes');

(async () => {
  const t = fakes.checker();
  const db = await open();
  const who = await fakes.backends();
  const env = { DRIVER_URL: who.url, RIDER_URL: who.url };
  const deletion = require(path.join(__dirname, '..', 'stack', 'maps-shim', 'deletion.js'));

  const call = async (fn, token, body) => {
    const r = fakes.res();
    if (fn === 'request') await deletion.request(db.pool, env, token, body, r);
    else await deletion[fn](db.pool, env, token, r);
    return r;
  };
  const rows = (id) => db.sql('SELECT status, phone, reason, handled_by, delete_by, requested_at FROM movin.deletion_request WHERE person_id = $1 ORDER BY id', [id]);

  await db.sql("INSERT INTO atlas_driver_offer_bpp.person (id, merchant_id, unencrypted_mobile_number) VALUES ('d1', 'favorit0-0000-0000-0000-00000favorit', '22778899')");
  await db.sql("INSERT INTO atlas_app.person (id, unencrypted_mobile_number) VALUES ('p1', '0555000199')");
  who.drivers.set('tok-d1', 'd1');
  who.riders.set('tok-p1', 'p1');

  t.section('1. who is asking comes from the token, nothing else');
  let r = await call('status', '');
  t.ok(r.status === 401, 'no token: 401');
  r = await call('request', 'tok-forged', { reason: 'x', personId: 'd1' });
  t.ok(r.status === 401 && (await rows('d1')).length === 0, 'an unknown token, even naming someone in the body: 401, nothing recorded');

  t.section('2. a driver asks, sees it pending, cannot ask twice, withdraws');
  r = await call('status', 'tok-d1');
  t.ok(r.status === 200 && r.json().state === 'none', 'before: none');
  r = await call('request', 'tok-d1', { reason: `  ${'x'.repeat(500)}  ` });
  let d = (await rows('d1'))[0];
  t.ok(r.status === 200 && r.json().ok === true, 'requested: 200');
  t.ok(d.status === 'open' && d.phone === '22778899' && d.reason.length === 400,
    'recorded open, with his number for the office, the reason trimmed to 400', JSON.stringify({ ...d, reason: d.reason.length }));
  const days = (new Date(d.delete_by) - new Date(d.requested_at)) / 86400000;
  t.ok(Math.abs(days - 30) < 0.01, `the office has 30 days (${days.toFixed(2)})`);
  r = await call('status', 'tok-d1');
  t.ok(r.json().state === 'pending' && r.json().deleteBy, 'status: pending, with the date');
  r = await call('request', 'tok-d1', {});
  t.ok(r.status === 409 && r.json().blocker === 'already_requested' && (await rows('d1')).length === 1,
    'a second tap: 409 already_requested, still one row');
  const [a, b] = await Promise.all([call('request', 'tok-p1', {}), call('request', 'tok-p1', {})]);
  t.ok((await rows('p1')).filter((x) => x.status === 'open').length === 1 && [a.status, b.status].includes(200),
    'two taps at once on a slow connection: one open request (the partial unique index)', `${a.status}/${b.status}`);
  r = await call('withdraw', 'tok-d1');
  d = (await rows('d1'))[0];
  t.ok(r.json().ok === true && d.status === 'withdrawn' && d.handled_by === 'self',
    'withdrawn: the row stays, marked withdrawn by himself -- the office sees he changed his mind');
  r = await call('request', 'tok-d1', {});
  t.ok(r.status === 200 && (await rows('d1')).length === 2, 'and he may ask again later');
  await call('withdraw', 'tok-d1');

  t.section('3. never while a ride is under way -- and an unknown state counts as under way');
  const ride = (id, status) => db.sql('INSERT INTO atlas_driver_offer_bpp.ride (id, driver_id, status) VALUES ($1, $2, $3)', [id, 'd1', status]);
  await ride('r1', 'COMPLETED');
  await ride('r2', 'CANCELLED');
  r = await call('status', 'tok-d1');
  t.ok(r.json().state === 'none', 'only finished rides: may ask');
  await ride('r3', 'INPROGRESS');
  r = await call('request', 'tok-d1', {});
  t.ok(r.status === 409 && r.json().blocker === 'active_ride', 'a ride in progress: 409 active_ride');
  await db.sql("UPDATE atlas_driver_offer_bpp.ride SET status = 'SOME_STATE_NOBODY_HAS_SEEN' WHERE id = 'r3'");
  r = await call('status', 'tok-d1');
  t.ok(r.json().state === 'blocked', 'a status nobody here has seen: blocked (the safe direction)');
  await db.sql("UPDATE atlas_driver_offer_bpp.ride SET status = 'COMPLETED' WHERE id = 'r3'");
  await db.sql("INSERT INTO atlas_driver_offer_bpp.driver_information (driver_id, on_ride) VALUES ('d1', true)");
  r = await call('status', 'tok-d1');
  t.ok(r.json().state === 'blocked', 'rides all finished but the dispatcher\'s on_ride flag is up: blocked');
  await db.sql("UPDATE atlas_driver_offer_bpp.driver_information SET on_ride = false WHERE driver_id = 'd1'");
  db.pool.failWhen = /FROM atlas_driver_offer_bpp\.ride/;
  r = await call('request', 'tok-d1', {});
  t.ok(r.status === 409 && r.json().blocker === 'active_ride', 'the ride check itself fails: refused, never silently allowed');

  t.section('4. the passenger side reads its own rides, through the booking');
  await call('withdraw', 'tok-p1');
  await db.sql("INSERT INTO atlas_app.booking (id, rider_id) VALUES ('b1', 'p1')");
  await db.sql("INSERT INTO atlas_app.ride (id, booking_id, status) VALUES ('pr1', 'b1', 'NEW')");
  r = await call('status', 'tok-p1');
  t.ok(r.json().state === 'blocked', 'a passenger with a ride under way: blocked');
  await db.sql("UPDATE atlas_app.ride SET status = 'COMPLETED' WHERE id = 'pr1'");
  r = await call('request', 'tok-p1', { reason: 'moving away' });
  const p = (await rows('p1')).filter((x) => x.status === 'open')[0];
  t.ok(r.status === 200 && p && p.phone === '0555000199' && p.reason === 'moving away',
    'finished: recorded, with the passenger\'s own number');
  const sides = await db.sql("SELECT side FROM movin.deletion_request WHERE person_id = 'p1' AND status = 'open'");
  t.ok(sides.length === 1 && sides[0].side === 'rider', 'recorded as the rider side, from the token that was accepted');

  who.close(); await db.close();
  t.finish();
})().catch((e) => { console.error(e); process.exit(2); });
