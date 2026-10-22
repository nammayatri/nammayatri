'use strict';
// Who dispatch skips: maps-shim/restricted.js, its SQL run for real (PGlite).
// Phase 5 (2026-10-06). The list it publishes is what the binary's
// `movinOnlyPaying` reads -- a driver on it is never offered a ride.
//
//   cd Backend/dev/local-stack/tests && npm ci && node restricted.test.js
//
// The rule (client, 2026-09-14): restricted unless he has an active day or
// the credit to open one, at HIS country's price; and, inside a paid day,
// once he reaches the ride cap. Every failure leaves the last list in place.
const path = require('path');
const { open } = require('./lib/db');
const fakes = require('./lib/fakes');

const MR = 'favorit0-0000-0000-0000-00000favorit';
const DZ = 'algeria0-0000-0000-0000-00000algeria';
const HOUR = 3600 * 1000;
const CAP = 3;

(async () => {
  const t = fakes.checker();
  const db = await open();
  const redis = await fakes.redis();
  Object.assign(process.env, {
    REDIS_HOST: '127.0.0.1', REDIS_PORT: String(redis.port),
    // Small, so the cap is reachable with a handful of rows.
    SUBSCRIPTION_RIDE_CAP: String(CAP),
  });
  const restricted = require(path.join(__dirname, '..', 'stack', 'maps-shim', 'restricted.js'));

  const person = async (id, merchant, { balance, dayUntil, role = 'DRIVER' } = {}) => {
    await db.sql('INSERT INTO atlas_driver_offer_bpp.person (id, merchant_id, role) VALUES ($1, $2, $3)', [id, merchant, role]);
    if (balance !== undefined || dayUntil !== undefined) {
      await db.sql('INSERT INTO movin.wallet (driver_id, balance, day_until) VALUES ($1, $2, $3)', [id, balance || 0, dayUntil || null]);
    }
  };
  const inDay = new Date(Date.now() + 10 * HOUR);
  const ended = new Date(Date.now() - HOUR);

  // name, merchant, wallet, restricted?
  const cases = [
    ['mr-never-topped-up', MR, {}, true],
    ['mr-29', MR, { balance: 29 }, true],
    ['mr-30', MR, { balance: 30 }, false],
    ['mr-0-in-day', MR, { balance: 0, dayUntil: inDay }, false],
    ['mr-neg-in-day', MR, { balance: -30, dayUntil: inDay }, false],
    ['mr-0-day-ended', MR, { balance: 0, dayUntil: ended }, true],
    ['mr-30-day-ended', MR, { balance: 30, dayUntil: ended }, false],
    ['dz-40', DZ, { balance: 40 }, true],
    ['dz-99', DZ, { balance: 99 }, true],
    ['dz-100', DZ, { balance: 100 }, false],
    ['dz-0-in-day', DZ, { balance: 0, dayUntil: inDay }, false],
    ['admin-no-wallet', MR, { role: 'ADMIN' }, false],
  ];
  for (const [id, m, w] of cases) await person(id, m, w);

  t.section('1. the list, driver by driver');
  const ids = await restricted.refresh(db.pool, 'test');
  for (const [id, , , want] of cases) {
    t.ok(ids.includes(id) === want, `${id}: ${want ? 'skipped by dispatch' : 'gets rides'}`);
  }

  t.section('2. the ride cap, inside a paid day');
  await person('mr-capped', MR, { balance: 500, dayUntil: inDay });
  await person('mr-under-cap', MR, { balance: 500, dayUntil: inDay });
  let n = 0;
  const rides = async (driver, count, status, hoursAgo) => {
    for (let i = 0; i < count; i += 1) {
      n += 1;
      await db.sql('INSERT INTO atlas_driver_offer_bpp.ride (id, driver_id, status, created_at) VALUES ($1, $2, $3, $4)',
        [`r${n}`, driver, status, new Date(Date.now() - hoursAgo * HOUR)]);
    }
  };
  await rides('mr-capped', CAP, 'COMPLETED', 2);
  await rides('mr-under-cap', CAP - 1, 'COMPLETED', 2);
  await rides('mr-under-cap', 5, 'CANCELLED', 2);
  await rides('mr-under-cap', 5, 'COMPLETED', 30); // before this day began
  let list = await restricted.refresh(db.pool, 'test');
  t.ok(list.includes('mr-capped'), `${CAP} completed rides in the day: skipped (cap ${CAP})`);
  t.ok(!list.includes('mr-under-cap'),
    `${CAP - 1} in the day, plus cancelled ones and yesterday's: still gets rides`);

  t.section('3. published under both keys, and failure keeps the last list');
  const unpaid = JSON.parse(redis.values.get(restricted.KEY_UNPAID));
  const old = JSON.parse(redis.values.get(restricted.KEY));
  t.ok(restricted.KEY_UNPAID === 'dynamic-offer-driver-app:movin:unpaid', 'the key the binary reads, prefix included');
  t.ok(JSON.stringify(unpaid.sort()) === JSON.stringify([...list].sort()) && JSON.stringify(old.sort()) === JSON.stringify(unpaid),
    'both keys hold exactly the computed list');
  const setsBefore = redis.sets;
  db.pool.failNext = 1;
  const r = await restricted.refresh(db.pool, 'test');
  t.ok(r === null && redis.sets === setsBefore, 'the query fails: nothing published, the last list stays');
  redis.down = true;
  list = await restricted.refresh(db.pool, 'test');
  redis.down = false;
  t.ok(Array.isArray(list), 'Redis refuses: the shim carries on (dispatch keeps the last list)');
  t.ok((await restricted.refresh(null)) === null, 'no database at all: nothing published, nobody restricted by us');

  t.section('4. the top-up that clears him');
  await db.sql("UPDATE movin.wallet SET balance = 30 WHERE driver_id = 'mr-29'");
  list = await restricted.refresh(db.pool, 'top-up credited');
  t.ok(!list.includes('mr-29') && !JSON.parse(redis.values.get(restricted.KEY_UNPAID)).includes('mr-29'),
    '29 -> 30 MRU: off the list on the next refresh');

  redis.close(); await db.close();
  t.finish();
})().catch((e) => { console.error(e); process.exit(2); });
