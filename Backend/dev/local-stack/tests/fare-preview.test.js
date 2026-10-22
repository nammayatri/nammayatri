'use strict';
// The price shown when no driver of that type is near: maps-shim/fare-preview.js.
// Its SQL runs for real (PGlite) against our own tariff files -- the numbers
// below are the tariff we sell, not invented ones -- and three are the live
// server's own quotes, read off the owner's phone on 2026-10-21/22 (Algiers,
// Voiture): 371-486, 571 and 899-1184 DA.
//
//   cd Backend/dev/local-stack/tests && npm ci && node fare-preview.test.js
const fs = require('fs');
const path = require('path');
const { open } = require('./lib/db');
const fakes = require('./lib/fakes');

const STACK = path.join(__dirname, '..', 'stack');
const MR = 'favorit0-0000-0000-0000-00000favorit';
const DZ = 'algeria0-0000-0000-0000-00000algeria';
const VARIANTS = ['SEDAN', 'SUV', 'HATCHBACK', 'AUTO_RICKSHAW'];

(async () => {
  const c = fakes.checker();
  const t = {
    check: (label, cond, detail) => c.ok(Boolean(cond), label, detail === undefined ? '' : JSON.stringify(detail)),
    done: () => c.finish(),
  };
  const { farePreview, priceOf, roundHalfEven } = require(path.join(STACK, 'maps-shim', 'fare-preview.js'));

  // ── Haskell's round, which the provider uses ─────────────────────────────
  t.check('half to even: 165.5 -> 166, 164.5 -> 164, 2.5 -> 2', roundHalfEven(165.5) === 166 && roundHalfEven(164.5) === 164 && roundHalfEven(2.5) === 2);
  t.check('and the rest to the nearest: 355.4 -> 355, 355.6 -> 356', roundHalfEven(355.4) === 355 && roundHalfEven(355.6) === 356);

  // ── the tables, cut down to what the preview reads, and our real tariff ──
  const db = await open();
  await db.exec(`
    CREATE TABLE atlas_driver_offer_bpp.merchant (id character(36) PRIMARY KEY);
    CREATE TABLE atlas_driver_offer_bpp.fare_policy (
      id text PRIMARY KEY, merchant_id character(36) NOT NULL, vehicle_variant text NOT NULL,
      base_distance_fare numeric, base_distance_meters integer, per_extra_km_fare numeric,
      dead_km_fare integer, driver_min_extra_fee integer, driver_max_extra_fee integer,
      night_shift_rate numeric DEFAULT 1, updated_at timestamptz);
    CREATE TABLE atlas_driver_offer_bpp.restricted_extra_fare (
      id text PRIMARY KEY, merchant_id character(36) NOT NULL, vehicle_variant text NOT NULL,
      min_trip_distance integer NOT NULL, driver_max_extra_fare integer NOT NULL);
  `);
  for (const m of [MR, DZ]) {
    await db.sql('INSERT INTO atlas_driver_offer_bpp.merchant (id) VALUES ($1)', [m]);
    for (const v of VARIANTS) {
      await db.sql('INSERT INTO atlas_driver_offer_bpp.fare_policy (id, merchant_id, vehicle_variant) VALUES ($1, $2, $3)', [`${m}-${v}`, m, v]);
    }
  }
  for (const f of ['algeria-tariff.sql', 'mauritania-tariff.sql']) {
    await db.exec(fs.readFileSync(path.join(STACK, 'db', f), 'utf8'));
  }

  // The geofence needs PostGIS, which PGlite has not: answered by longitude --
  // west of Greenwich is Mauritania, east is Algeria -- and every other query
  // goes to the real tables.
  const regionAsked = [];
  const pool = {
    query: (sql, params) => {
      if (/from atlas_app\.geometry/.test(sql)) {
        regionAsked.push(params);
        const lon = params[1];
        return Promise.resolve({ rows: lon > 100 ? [] : [{ region: lon < 0 ? 'Mauritania' : 'Algeria' }] });
      }
      return db.pool.query(sql, params);
    },
  };

  const ask = async (query, { signedIn = true } = {}) => {
    let status = 0;
    let body = null;
    const res = {
      writeHead: (s) => { status = s; },
      setHeader: () => {},
      end: (b) => { body = b ? JSON.parse(b) : null; },
    };
    await farePreview({ url: new URL(`http://x/fleet/fare-preview?${query}`), res, pool, isSignedIn: async () => signedIn });
    return { status, body };
  };
  const algiers = 'lat=36.75&lon=3.06';
  const nouakchott = 'lat=18.08&lon=-15.97';

  // ── the live server's own quotes, Algiers, Voiture ───────────────────────
  // Measured under the tariff of before 2026-10-22, when the Voiture started at
  // 150 DA: that start is put back for these three, then the file again.
  await db.sql(`UPDATE atlas_driver_offer_bpp.fare_policy SET base_distance_fare = 150 WHERE merchant_id = $1 AND vehicle_variant = 'SEDAN'`, [DZ]);
  let r = await ask(`${algiers}&distance=3120&variant=SEDAN`);
  t.check('3.12 km: 371-486 DA, as the server quoted it', r.status === 200 && r.body.min === 371 && r.body.max === 486, r);
  r = await ask(`${algiers}&distance=7120&variant=SEDAN`);
  t.check('7.12 km: 571 DA, the receipt (215 start + 356)', r.status === 200 && r.body.min === 571, r);
  r = await ask(`${algiers}&distance=13680&variant=SEDAN`);
  t.check('13.68 km: 899-1184 DA, as the server quoted it', r.status === 200 && r.body.min === 899 && r.body.max === 1184, r);

  // ── the tariff of 2026-10-22: the start is 25 DA (the boss) ──────────────
  await db.exec(fs.readFileSync(path.join(STACK, 'db', 'algeria-tariff.sql'), 'utf8'));
  r = await ask(`${algiers}&distance=7120&variant=SEDAN`);
  t.check('since 2026-10-22, 7.12 km Voiture: 25 + 65 + 356 = 446 DA, up to 631', r.status === 200 && r.body.min === 446 && r.body.max === 631, r);

  // ── the bands step at their lower bound, and the cap is the band's ───────
  r = await ask(`${algiers}&distance=1999&variant=SEDAN`);
  const under = r.body;
  r = await ask(`${algiers}&distance=2000&variant=SEDAN`);
  t.check('the 2 km band starts at 2000 m exactly (85 under it, 115 from it)', under.max - under.min === 85 && r.body.max - r.body.min === 115, [under, r.body]);

  // ── each country prices with its own merchant ────────────────────────────
  const mrRow = (await db.sql(`SELECT base_distance_fare::float8 AS base, per_extra_km_fare::float8 AS per_km, dead_km_fare AS dead
                                 FROM atlas_driver_offer_bpp.fare_policy WHERE merchant_id = $1 AND vehicle_variant = 'SEDAN'`, [MR]))[0];
  r = await ask(`${nouakchott}&distance=5000&variant=SEDAN`);
  const expected = Number(mrRow.dead) + roundHalfEven(Number(mrRow.base)) + roundHalfEven(5 * Number(mrRow.per_km));
  t.check(`Nouakchott prices from the Mauritanian rows (${expected} MRU), not the Algerian`, r.status === 200 && r.body.min === expected, [r, mrRow]);
  r = await ask(`${algiers}&distance=5000&variant=HATCHBACK`);
  t.check('another vehicle, its own row: Herbin 5 km = 25 + 50 + 175 = 250 DA', r.status === 200 && r.body.min === 250, r);

  // ── what it refuses ──────────────────────────────────────────────────────
  t.check('not signed in -> 401', (await ask(`${algiers}&distance=5000&variant=SEDAN`, { signedIn: false })).status === 401);
  t.check('unknown vehicle -> 400', (await ask(`${algiers}&distance=5000&variant=BUS`)).status === 400);
  t.check('no distance -> 400', (await ask(`${algiers}&variant=SEDAN`)).status === 400);
  t.check('a position off the map -> 400', (await ask('lat=95&lon=3&distance=5000&variant=SEDAN')).status === 400);
  t.check('outside both countries -> 404', (await ask('lat=10&lon=150&distance=5000&variant=SEDAN')).status === 404);
  await db.sql(`UPDATE atlas_driver_offer_bpp.fare_policy SET night_shift_rate = 1.5 WHERE merchant_id = $1 AND vehicle_variant = 'SUV'`, [DZ]);
  t.check('a night multiplier other than 1 -> 404, never a guess', (await ask(`${algiers}&distance=5000&variant=SUV`)).status === 404);
  t.check('the geofence is asked with (lat, lon), as places.js does', regionAsked[0][0] === 36.75 && regionAsked[0][1] === 3.06, regionAsked[0]);

  // ── the pure formula, alone ──────────────────────────────────────────────
  const p = { base: 150, base_m: 0, per_km: 50, dead: 65, max_extra: 300 };
  t.check('no band: the policy\'s own cap (300)', priceOf(p, null, 3300).max - priceOf(p, null, 3300).min === 300);
  t.check('a free stretch is not charged: base_distance_meters 3000, 2 km -> start only', priceOf({ ...p, base_m: 3000 }, 0, 2000).min === 215);

  t.done();
})().catch((e) => {
  console.error(e);
  process.exit(1);
});
