'use strict';
// stack/apply-tariff.sh, the real script, against a real Postgres (PGlite through
// lib/fake-docker.js). The owner, 2026-10-22: « never change one country without
// the other ». So it applies both tariff files by default, in ONE transaction:
// both countries move, or neither does.
//
//   cd Backend/dev/local-stack/tests && npm ci && node apply-tariff.test.js
const fs = require('fs');
const os = require('os');
const path = require('path');
const { spawn } = require('child_process');
const { open } = require('./lib/db');
const fakes = require('./lib/fakes');
const fakeDocker = require('./lib/fake-docker');

const STACK = path.join(__dirname, '..', 'stack');
const MR = 'favorit0-0000-0000-0000-00000favorit';
const DZ = 'algeria0-0000-0000-0000-00000algeria';
const VARIANTS = ['SEDAN', 'SUV', 'HATCHBACK', 'AUTO_RICKSHAW'];

(async () => {
  const c = fakes.checker();
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
      // The tariff before 2026-10-22, so a change is visible.
      const start = m === DZ ? (v === 'SUV' ? 200 : v === 'SEDAN' ? 150 : 100) : (v === 'SUV' ? 60 : v === 'SEDAN' ? 45 : 30);
      await db.sql('INSERT INTO atlas_driver_offer_bpp.fare_policy (id, merchant_id, vehicle_variant, base_distance_fare) VALUES ($1, $2, $3, $4)', [`${m}-${v}`, m, v, start]);
    }
  }
  const starts = async () => {
    const rows = await db.sql('SELECT merchant_id, vehicle_variant, base_distance_fare::float8 AS s, per_extra_km_fare::float8 AS km, dead_km_fare AS dead FROM atlas_driver_offer_bpp.fare_policy');
    const out = {};
    for (const r of rows) out[`${r.merchant_id.trim() === DZ ? 'DZ' : 'MR'} ${r.vehicle_variant}`] = r;
    return out;
  };
  const fd = await fakeDocker.start(db);
  // Async, never spawnSync: the fake docker answers from THIS process, so a
  // blocking spawn would wait forever on its own server.
  const run = (script, args = []) => new Promise((resolve) => {
    const p = spawn('bash', [script, ...args], {
      env: { ...process.env, PATH: `${fd.bin}:${process.env.PATH}` },
      stdio: ['ignore', 'pipe', 'pipe'],
    });
    let stdout = '';
    let stderr = '';
    p.stdout.on('data', (d) => { stdout += d; });
    p.stderr.on('data', (d) => { stderr += d; });
    p.on('close', (status) => resolve({ status, stdout: stdout.replace(/\x1b\[[0-9;]*m/g, ''), stderr }));
  });

  // ── the default: both countries, the start 25 DA / 8 MRU everywhere ──────
  let r = await run(path.join(STACK, 'apply-tariff.sh'));
  c.ok(r.status === 0, 'apply-tariff.sh with no argument -> exit 0', r.stdout.slice(-400) + r.stderr.slice(-400));
  c.ok(/in one transaction/.test(r.stdout), 'it says both files went in one transaction', r.stdout.slice(0, 300));
  let now = await starts();
  c.ok(VARIANTS.every((v) => now[`DZ ${v}`].s === 25), 'Algeria: every start is 25 DA', JSON.stringify(now));
  c.ok(VARIANTS.every((v) => now[`MR ${v}`].s === 8), 'Mauritania: every start is 8 MRU, the same day', JSON.stringify(now));
  c.ok(now['DZ SEDAN'].km === 50 && now['DZ SEDAN'].dead === 65 && now['MR SEDAN'].km === 15 && now['MR SEDAN'].dead === 20,
    'the per-km and the approach are unchanged (Voiture 50 + 65 DA, 15 + 20 MRU)', JSON.stringify([now['DZ SEDAN'], now['MR SEDAN']]));
  const bands = await db.sql('SELECT merchant_id, count(*)::int AS n FROM atlas_driver_offer_bpp.restricted_extra_fare GROUP BY 1 ORDER BY 1');
  c.ok(bands.length === 2 && bands.every((b) => b.n === 40), 'both countries keep their ten bands per vehicle', JSON.stringify(bands));
  const scans = fd.redis.filter((a) => a[0] === '--scan').map((a) => a[2]);
  c.ok(scans.includes('driver-offer:CachedQueries:FarePolicy:*') && scans.includes('driver-offer:CachedQueries:RestrictExtraFee:*'),
    'both fare caches are cleared afterwards', JSON.stringify(scans));

  // ── a broken second file: the first one is rolled back with it ───────────
  await db.sql('UPDATE atlas_driver_offer_bpp.fare_policy SET base_distance_fare = 150 WHERE merchant_id = $1', [DZ]);
  const box = fs.mkdtempSync(path.join(os.tmpdir(), 'tariff-'));
  fs.mkdirSync(path.join(box, 'db'));
  fs.copyFileSync(path.join(STACK, 'apply-tariff.sh'), path.join(box, 'apply-tariff.sh'));
  fs.copyFileSync(path.join(STACK, 'db', 'algeria-tariff.sql'), path.join(box, 'db', 'algeria-tariff.sql'));
  fs.writeFileSync(path.join(box, 'db', 'mauritania-tariff.sql'),
    fs.readFileSync(path.join(STACK, 'db', 'mauritania-tariff.sql'), 'utf8').replace('COMMIT;', 'SELECT no_such_column FROM atlas_driver_offer_bpp.fare_policy;\nCOMMIT;'));
  r = await run(path.join(box, 'apply-tariff.sh'));
  now = await starts();
  c.ok(r.status === 1, 'a failing Mauritanian file -> exit 1', r.stdout.slice(-300));
  c.ok(VARIANTS.every((v) => now[`DZ ${v}`].s === 150), 'and Algeria is NOT changed alone: its 150 stayed', JSON.stringify(now));
  fs.rmSync(box, { recursive: true, force: true });

  // ── one file named on purpose still works, for that country only ─────────
  r = await run(path.join(STACK, 'apply-tariff.sh'), [path.join(STACK, 'db', 'algeria-tariff.sql')]);
  now = await starts();
  c.ok(r.status === 0 && now['DZ SEDAN'].s === 25, 'a file named explicitly is applied (Algeria back to 25)', r.stdout.slice(-300));

  await fd.stop();
  c.finish();
})().catch((e) => {
  console.error(e);
  process.exit(1);
});
