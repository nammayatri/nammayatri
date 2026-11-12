'use strict';
// stack/key-rotation/rotate.js against a real Postgres (PGlite) and two fake
// passettos holding different keys: what the rotation reads, refuses, writes
// and proves.
//
//   cd Backend/dev/local-stack/tests && npm ci && node key-rotation.test.js
//
//   1. check     -- reads every value, proves today's salt, writes nothing
//   2. refusals  -- a hash today's salt does not reproduce; a salt unchanged
//   3. apply     -- new ciphertext and hashes, readable only with the new keys,
//                   an empty hash left empty, the photographs' renames
const http = require('http');
const fs = require('fs');
const os = require('os');
const path = require('path');
const { open } = require('./lib/db');
const fakes = require('./lib/fakes');
const rotate = require(path.join(__dirname, '..', 'stack', 'key-rotation', 'rotate.js'));

/** A passetto holding one key: it decrypts only what it encrypted. */
async function passetto(key) {
  const server = http.createServer((req, res) => {
    let raw = '';
    req.on('data', (c) => (raw += c));
    req.on('end', () => {
      const { value } = JSON.parse(raw || '{}');
      const send = (status, body) => {
        res.writeHead(status, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify(body));
      };
      if (req.url === '/encrypt') {
        if (!/^S".*"$/.test(value)) return send(400, { error: 'not a shown string' });
        return send(200, { value: `0.1.0|0|${Buffer.from(`${key}|${value}`).toString('base64')}` });
      }
      if (req.url === '/decrypt') {
        const inner = Buffer.from(String(value).split('|')[2] || '', 'base64').toString();
        if (!inner.startsWith(`${key}|`)) return send(500, { error: 'cannot decrypt' });
        return send(200, { value: inner.slice(key.length + 1) });
      }
      send(404, {});
    });
  });
  await new Promise((r) => server.listen(0, '127.0.0.1', r));
  const url = `http://127.0.0.1:${server.address().port}`;
  return { url, vault: rotate.vault(url), close: () => server.close() };
}

const TABLES = `
  ALTER TABLE atlas_app.person
    ADD COLUMN mobile_number_encrypted text, ADD COLUMN mobile_number_hash bytea,
    ADD COLUMN email_encrypted text, ADD COLUMN email_hash bytea;
  ALTER TABLE atlas_driver_offer_bpp.person
    ADD COLUMN mobile_number_encrypted text, ADD COLUMN mobile_number_hash bytea;
  CREATE TABLE atlas_driver_offer_bpp.ride_details
    (id text PRIMARY KEY, driver_number_encrypted text, driver_number_hash bytea);
  CREATE TABLE atlas_driver_offer_bpp.rider_details
    (id text PRIMARY KEY, mobile_number_encrypted text, mobile_number_hash bytea);
  CREATE TABLE atlas_driver_offer_bpp.driver_license
    (id text PRIMARY KEY, license_number_encrypted text, license_number_hash bytea);
  CREATE TABLE atlas_driver_offer_bpp.idfy_verification
    (id text PRIMARY KEY, document_number_encrypted text, document_number_hash bytea);
  CREATE TABLE atlas_driver_offer_bpp.vehicle_registration_certificate
    (id text PRIMARY KEY, certificate_number_encrypted text, certificate_number_hash bytea);
  CREATE TABLE atlas_app.merchant_service_config
    (merchant_id character(36), service_name text, config_json json);
  CREATE TABLE atlas_driver_offer_bpp.merchant_service_config
    (merchant_id character(36), service_name text, config_json json);
`;

(async () => {
  const t = fakes.checker();
  const db = await open();
  for (const stmt of TABLES.split(';').filter((s) => s.trim())) await db.sql(stmt);

  const oldKeys = await passetto('old-key');
  const newKeys = await passetto('new-key');
  const OLD = 'today-salt';
  const NEW = 'tomorrow-salt';
  const enc = (p) => oldKeys.vault.encrypt(p);
  const h = (salt, p) => rotate.hashOf(salt, p);

  // Two passengers (one with a photograph), a driver, a ride, and two
  // rider_details rows: one hashed, one from before hashes were written.
  await db.sql(
    `INSERT INTO atlas_app.person (id, mobile_number_encrypted, mobile_number_hash)
     VALUES ('r1', $1, $2), ('r2', $3, $4)`,
    [await enc('22334455'), h(OLD, '22334455'), await enc('0555000199'), h(OLD, '0555000199')],
  );
  await db.sql(
    `INSERT INTO atlas_driver_offer_bpp.person (id, merchant_id, mobile_number_encrypted, mobile_number_hash)
     VALUES ('d1', 'favorit0-0000-0000-0000-00000favorit', $1, $2)`,
    [await enc('22100001'), h(OLD, '22100001')],
  );
  await db.sql(
    `INSERT INTO atlas_driver_offer_bpp.ride_details (id, driver_number_encrypted, driver_number_hash)
     VALUES ('rd1', $1, $2)`,
    [await enc('22100001'), h(OLD, '22100001')],
  );
  await db.sql(
    `INSERT INTO atlas_driver_offer_bpp.rider_details (id, mobile_number_encrypted, mobile_number_hash)
     VALUES ('x1', $1, $2), ('x2', $3, NULL), ('x3', 'deleted:p-erased', NULL)`,
    [await enc('22334455'), h(OLD, '22334455'), await enc('22990011')],
  );

  /* ── 1. check ─────────────────────────────────────────────────────────── */
  t.section('1. check: every value read, today\'s salt proven, nothing written');
  const before = await db.sql('SELECT mobile_number_encrypted AS e FROM atlas_app.person ORDER BY id');
  const checked = await rotate.check({ pool: db.pool, oldVault: oldKeys.vault, oldSalt: OLD });
  t.ok(checked.rows.length === 6, 'six values read', String(checked.rows.length));
  t.ok(checked.erased.length === 1, 'an erased account\'s marker is set aside, not decrypted');
  t.ok(checked.counts.get('atlas_driver_offer_bpp.rider_details.mobile_number').hashes === 1,
    'rider_details: two values, one hash -- the unhashed row is counted as such');
  const after = await db.sql('SELECT mobile_number_encrypted AS e FROM atlas_app.person ORDER BY id');
  t.ok(JSON.stringify(before) === JSON.stringify(after), 'check wrote nothing');

  /* ── 2. refusals ──────────────────────────────────────────────────────── */
  t.section('2. it stops before writing when the method is wrong');
  await db.sql(`INSERT INTO atlas_driver_offer_bpp.rider_details VALUES ('x4', 'deleted:p-odd', $1)`, [h(OLD, 'x')]);
  let odd = '';
  try { await rotate.check({ pool: db.pool, oldVault: oldKeys.vault, oldSalt: OLD }); } catch (e) { odd = e.message; }
  t.ok(/erased value with a hash/.test(odd), 'an erased marker that still has a hash stops it', odd);
  await db.sql(`DELETE FROM atlas_driver_offer_bpp.rider_details WHERE id = 'x4'`);
  let stopped = '';
  try {
    await rotate.check({ pool: db.pool, oldVault: oldKeys.vault, oldSalt: 'a-wrong-salt' });
  } catch (e) { stopped = e.message; }
  t.ok(/does not reproduce its hash/.test(stopped), 'a salt that does not reproduce the hashes stops it', stopped);
  stopped = '';
  try {
    await rotate.apply({ pool: db.pool, oldVault: oldKeys.vault, newVault: newKeys.vault, oldSalt: OLD, newSalt: OLD });
  } catch (e) { stopped = e.message; }
  t.ok(/new salt/.test(stopped), 'the same salt twice is refused', stopped);
  stopped = '';
  try {
    await rotate.check({ pool: db.pool, oldVault: newKeys.vault, oldSalt: OLD });
  } catch (e) { stopped = e.message; }
  t.ok(/decrypt answered 500/.test(stopped), 'values the given keys cannot open stop it', stopped);
  const still = await db.sql('SELECT mobile_number_encrypted AS e FROM atlas_app.person ORDER BY id');
  t.ok(JSON.stringify(before) === JSON.stringify(still), 'and none of that wrote anything');

  /* ── 3. apply ─────────────────────────────────────────────────────────── */
  t.section('3. apply: new keys, new hashes, read back, photographs');
  const photos = fs.mkdtempSync(path.join(os.tmpdir(), 'avatars-'));
  const r1Photo = 'h_' + h(OLD, '22334455').toString('hex').slice(0, 32) + '.jpg';
  fs.writeFileSync(path.join(photos, r1Photo), 'jpeg');

  const done = await rotate.apply({
    pool: db.pool, oldVault: oldKeys.vault, newVault: newKeys.vault, oldSalt: OLD, newSalt: NEW,
  });
  t.ok(done.rows.length === 6, 'six values rewritten');

  const [r1] = await db.sql(`SELECT mobile_number_encrypted AS e, mobile_number_hash AS h FROM atlas_app.person WHERE id = 'r1'`);
  t.ok((await newKeys.vault.decrypt(r1.e)) === '22334455', 'a passenger\'s number opens with the new keys');
  let oldOpens = true;
  try { await oldKeys.vault.decrypt(r1.e); } catch { oldOpens = false; }
  t.ok(!oldOpens, '... and no longer with the old ones');
  t.ok(Buffer.from(r1.h).equals(h(NEW, '22334455')), 'her hash is the new salt\'s');

  const [x3] = await db.sql(`SELECT mobile_number_encrypted AS e, mobile_number_hash AS h FROM atlas_driver_offer_bpp.rider_details WHERE id = 'x3'`);
  t.ok(x3.e === 'deleted:p-erased' && x3.h === null, 'an erased account\'s row is exactly as it was');
  const [x2] = await db.sql(`SELECT mobile_number_hash AS h FROM atlas_driver_offer_bpp.rider_details WHERE id = 'x2'`);
  t.ok(x2.h === null, 'a row that had no hash still has none');
  const [d1] = await db.sql(`SELECT driver_number_encrypted AS e FROM atlas_driver_offer_bpp.ride_details WHERE id = 'rd1'`);
  t.ok((await newKeys.vault.decrypt(d1.e)) === '22100001', 'a ride\'s driver number opens with the new keys');

  // The same number, hashed on both sides, still matches itself: the
  // passenger's rating (rider_details) follows her person row.
  const [x1] = await db.sql(`SELECT mobile_number_hash AS h FROM atlas_driver_offer_bpp.rider_details WHERE id = 'x1'`);
  t.ok(Buffer.from(x1.h).equals(Buffer.from(r1.h)), 'her rider_details hash still equals her person hash');

  t.ok(done.renames.length === 2, 'two passengers\' photographs to rename', String(done.renames.length));
  const moved = rotate.renamePhotos(photos, done.renames);
  const newPhoto = 'h_' + h(NEW, '22334455').toString('hex').slice(0, 32) + '.jpg';
  t.ok(moved.length === 1 && fs.existsSync(path.join(photos, newPhoto)) && !fs.existsSync(path.join(photos, r1Photo)),
    'the one photograph moved to her new key; the passenger without one is skipped');

  /* ── 4. configs ───────────────────────────────────────────────────────── */
  t.section('4. configs: the encrypted strings inside the services\' JSON');
  const sealed = await oldKeys.vault.encryptRaw('S"maps-api-key"');
  const nested = await oldKeys.vault.encryptRaw('S"sms-password"');
  await db.sql(
    `INSERT INTO atlas_app.merchant_service_config VALUES ('m1', 'Maps_Google', $1::json),
            ('m1', 'Maps_OSRM', '{"osrmUrl":"http://osrm"}'::json)`,
    [JSON.stringify({ googleKey: sealed, googleMapsUrl: 'http://shim', sms: { password: nested, n: 3 } })],
  );
  const conf = await rotate.configs({ pool: db.pool, oldVault: oldKeys.vault, newVault: newKeys.vault });
  t.ok(conf.rows === 1 && conf.values === 2, 'one row, two encrypted values -- a row with none is left alone',
    JSON.stringify(conf));
  const [row] = await db.sql(`SELECT config_json AS j FROM atlas_app.merchant_service_config WHERE service_name = 'Maps_Google'`);
  const j = typeof row.j === 'string' ? JSON.parse(row.j) : row.j;
  t.ok((await newKeys.vault.decryptRaw(j.googleKey)) === 'S"maps-api-key"', 'the key opens with the new keys');
  t.ok((await newKeys.vault.decryptRaw(j.sms.password)) === 'S"sms-password"', '... and so does a nested one');
  t.ok(j.googleMapsUrl === 'http://shim' && j.sms.n === 3, 'everything else in the JSON is as it was');
  let openedOld = true;
  try { await oldKeys.vault.decryptRaw(j.googleKey); } catch { openedOld = false; }
  t.ok(!openedOld, 'the old keys no longer open it');

  oldKeys.close();
  newKeys.close();
  await db.close();
  t.finish();
})();
