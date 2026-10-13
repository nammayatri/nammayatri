'use strict';
/**
 * Rotation of the keys that encrypt stored numbers, and of the salt that
 * hashes them. Run once, in a maintenance window, after a rehearsal on a
 * restored copy (README.md beside this file).
 *
 *   node rotate.js check   read every encrypted value through the CURRENT
 *                          passetto, and prove the CURRENT salt reproduces
 *                          every stored hash. Writes nothing.
 *   node rotate.js apply   the same, then each value encrypted by the NEW
 *                          passetto and hashed with the NEW salt, all rows in
 *                          ONE transaction; then every new value read back
 *                          through the new passetto and compared. Prints the
 *                          passenger photographs to rename (their file name is
 *                          the hash), and renames them with --avatars DIR.
 *
 * Environment:
 *   DATABASE_URL       postgres://... of atlas_dev
 *   OLD_PASSETTO_URL   the passetto serving today's keys
 *   NEW_PASSETTO_URL   a passetto serving the new keys          (apply only)
 *   OLD_SALT_FILE      a file holding today's salt, nothing else
 *   NEW_SALT_FILE      a file holding the new salt               (apply only)
 * Salts are read from files so they never appear in a command line or a log.
 *
 * ── What it refuses ─────────────────────────────────────────────────────────
 *   * any stored hash the old salt does not reproduce: the method would be
 *     wrong, and every row it wrote would be wrong with it;
 *   * any value passetto will not decrypt, or that does not read back;
 *   * a new salt equal to the old one.
 * Nothing is written until every row has been read, checked and re-encrypted.
 *
 * A hash that was empty stays empty: rider_details rows written before the
 * backend stored hashes have none, and inventing one would change who they
 * match.
 *
 * An erased account's value is not ciphertext: the website's
 * db/deletion/anonymise.sql writes `deleted:<id>` into the columns that may
 * not be NULL, and clears the hash. Those rows are left exactly as they are --
 * and if one ever carries a hash, the rotation stops, because then it is not
 * what that script writes.
 */
const crypto = require('crypto');
const fs = require('fs');
const path = require('path');

/** Every column pair that holds an encrypted value and its hash, today. */
const COLUMNS = [
  ['atlas_app.person', 'mobile_number'],
  ['atlas_app.person', 'email'],
  ['atlas_driver_offer_bpp.person', 'mobile_number'],
  ['atlas_driver_offer_bpp.ride_details', 'driver_number'],
  ['atlas_driver_offer_bpp.rider_details', 'mobile_number'],
  ['atlas_driver_offer_bpp.driver_license', 'license_number'],
  ['atlas_driver_offer_bpp.idfy_verification', 'document_number'],
  ['atlas_driver_offer_bpp.vehicle_registration_certificate', 'certificate_number'],
];

/**
 * Configuration that holds encrypted strings inside JSON: the services'
 * credentials (maps, SMS, WhatsApp). Found AFTER the first switch on the live
 * server (2026-10-13): numbers were converted, these were not, and a ride
 * search failed with passetto's /decrypt/obj refusing them. A row is found by
 * its merchant and service, and only rewritten if it is still exactly what
 * was read.
 */
const CONFIGS = ['atlas_app.merchant_service_config', 'atlas_driver_offer_bpp.merchant_service_config'];
const SEALED = /^\d+\.\d+\.\d+\|\d+\|/;

/** Every encrypted string in the configs, re-encrypted from the old keys to the new ones. */
async function configs({ pool, oldVault, newVault }) {
  const rows = [];
  let values = 0;
  for (const table of CONFIGS) {
    const { rows: found } = await pool.query(
      `SELECT merchant_id, service_name, config_json::text AS j FROM ${table}
        WHERE config_json::text ~ '\\d+\\.\\d+\\.\\d+\\|\\d+\\|'`,
    );
    for (const r of found) {
      const walk = async (v) => {
        if (typeof v === 'string' && SEALED.test(v)) {
          const shown = await oldVault.decryptRaw(v).catch((e) => {
            throw new Error(`${table} ${r.service_name}: ${e.message}`);
          });
          const sealed = await newVault.encryptRaw(shown);
          if ((await newVault.decryptRaw(sealed)) !== shown) {
            throw new Error(`${table} ${r.service_name}: does not read back through the new keys`);
          }
          values += 1;
          return sealed;
        }
        if (Array.isArray(v)) return Promise.all(v.map(walk));
        if (v && typeof v === 'object') {
          const out = {};
          for (const [k, x] of Object.entries(v)) out[k] = await walk(x);
          return out;
        }
        return v;
      };
      rows.push({ table, ...r, next: JSON.stringify(await walk(JSON.parse(r.j))) });
    }
  }
  const client = typeof pool.connect === 'function' ? await pool.connect() : pool;
  try {
    await client.query('BEGIN');
    for (const r of rows) {
      const done = await client.query(
        `UPDATE ${r.table} SET config_json = $1::json
          WHERE merchant_id = $2 AND service_name = $3 AND config_json::text = $4`,
        [r.next, r.merchant_id, r.service_name, r.j],
      );
      if (done.rowCount !== 1) throw new Error(`${r.table} ${r.service_name}: changed while converting`);
    }
    await client.query('COMMIT');
  } catch (err) {
    await client.query('ROLLBACK');
    throw err;
  } finally {
    if (client !== pool && typeof client.release === 'function') client.release();
  }
  return { rows: rows.length, values };
}

/** The passenger's photograph is named after this hash (maps-shim/avatars.js). */
const PHOTO_HASH = 'atlas_app.person.mobile_number';

const hashOf = (salt, plain) => crypto.createHash('sha256').update(salt + plain).digest();

/**
 * passetto wraps a value as Haskell's `show` of a string: S"..." with
 * backslash escapes. Numbers and addresses are plain ASCII; anything else is
 * refused rather than guessed at.
 */
function wrap(plain) {
  if (!/^[\x20-\x7e]*$/.test(plain)) throw new Error('refusing a value with non-ASCII characters');
  return `S"${plain.replace(/\\/g, '\\\\').replace(/"/g, '\\"')}"`;
}
function unwrap(shown) {
  const m = /^S"((?:[^"\\]|\\["\\])*)"$/.exec(shown);
  if (!m) throw new Error('passetto answered a value this script cannot read');
  return m[1].replace(/\\(["\\])/g, '$1');
}

/** A passetto at `url`: POST /encrypt and /decrypt, {"value": ...} both ways. */
function vault(url) {
  const call = async (what, value) => {
    const res = await fetch(`${url.replace(/\/$/, '')}/${what}`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ value }),
    });
    if (!res.ok) {
      const why = (await res.text().catch(() => '')).slice(0, 160).replace(/\s+/g, ' ');
      throw new Error(`passetto ${what} answered ${res.status}${why ? ` (${why})` : ''}`);
    }
    const body = await res.json();
    if (typeof body.value !== 'string') throw new Error(`passetto ${what} answered no value`);
    return body.value;
  };
  return {
    decrypt: async (cipher) => unwrap(await call('decrypt', cipher)),
    encrypt: async (plain) => call('encrypt', wrap(plain)),
    // As passetto keeps it, unwrapped: for values that are not plain strings.
    decryptRaw: (cipher) => call('decrypt', cipher),
    encryptRaw: (shown) => call('encrypt', shown),
  };
}

/** Every row with a value, read and checked against today's salt. */
async function read(pool, oldVault, oldSalt, erased = []) {
  const rows = [];
  for (const [table, field] of COLUMNS) {
    const { rows: found } = await pool.query(
      `SELECT id, ${field}_encrypted AS enc, ${field}_hash AS hash FROM ${table}
        WHERE ${field}_encrypted IS NOT NULL ORDER BY id`,
    );
    for (const r of found) {
      if (String(r.enc).startsWith('deleted:')) {
        if (r.hash !== null) throw new Error(`${table}.${field} ${r.id}: an erased value with a hash`);
        erased.push({ table, field, id: r.id });
        continue;
      }
      // Where it failed, and the value's shape -- its version and key fields,
      // its length -- never the value itself.
      const shape = `${String(r.enc).split('|').slice(0, 2).join('|')}|… (${String(r.enc).length} chars)`;
      const plain = await oldVault.decrypt(r.enc).catch((e) => {
        throw new Error(`${table}.${field} ${r.id} [${shape}]: ${e.message}`);
      });
      const hash = r.hash === null ? null : Buffer.from(r.hash);
      if (hash !== null && !hash.equals(hashOf(oldSalt, plain))) {
        throw new Error(`${table}.${field} ${r.id}: the current salt does not reproduce its hash`);
      }
      rows.push({ table, field, id: r.id, plain, hash });
    }
  }
  return rows;
}

function counts(rows) {
  const by = new Map();
  for (const r of rows) {
    const k = `${r.table}.${r.field}`;
    const c = by.get(k) ?? { values: 0, hashes: 0 };
    c.values += 1;
    if (r.hash !== null) c.hashes += 1;
    by.set(k, c);
  }
  return by;
}

async function check({ pool, oldVault, oldSalt }) {
  const erased = [];
  const rows = await read(pool, oldVault, oldSalt, erased);
  return { rows, erased, counts: counts(rows) };
}

async function apply({ pool, oldVault, newVault, oldSalt, newSalt }) {
  if (!newSalt || newSalt === oldSalt) throw new Error('the new salt is empty or the old one');
  const erased = [];
  const rows = await read(pool, oldVault, oldSalt, erased);

  // Everything computed before anything is written.
  for (const r of rows) {
    r.enc = await newVault.encrypt(r.plain);
    r.newHash = r.hash === null ? null : hashOf(newSalt, r.plain);
  }

  const client = typeof pool.connect === 'function' ? await pool.connect() : pool;
  try {
    await client.query('BEGIN');
    for (const r of rows) {
      await client.query(
        `UPDATE ${r.table} SET ${r.field}_encrypted = $1, ${r.field}_hash = $2 WHERE id = $3`,
        [r.enc, r.newHash, r.id],
      );
    }
    await client.query('COMMIT');
  } catch (err) {
    await client.query('ROLLBACK');
    throw err;
  } finally {
    if (client !== pool && typeof client.release === 'function') client.release();
  }

  // Read back: every value through the NEW passetto, every hash with the NEW salt.
  for (const r of rows) {
    const { rows: now } = await pool.query(
      `SELECT ${r.field}_encrypted AS enc, ${r.field}_hash AS hash FROM ${r.table} WHERE id = $1`,
      [r.id],
    );
    const got = now[0];
    if ((await newVault.decrypt(got.enc)) !== r.plain) {
      throw new Error(`${r.table}.${r.field} ${r.id}: does not read back through the new keys`);
    }
    const hash = got.hash === null ? null : Buffer.from(got.hash);
    if ((hash === null) !== (r.newHash === null) || (hash && !hash.equals(r.newHash))) {
      throw new Error(`${r.table}.${r.field} ${r.id}: its new hash did not land`);
    }
  }

  const renames = rows
    .filter((r) => `${r.table}.${r.field}` === PHOTO_HASH && r.hash !== null)
    .map((r) => ({
      from: 'h_' + r.hash.toString('hex').slice(0, 32),
      to: 'h_' + r.newHash.toString('hex').slice(0, 32),
    }));
  return { rows, erased, counts: counts(rows), renames };
}

/** The photographs' moves, as maps-shim/avatars.js names them. Returns what moved. */
function renamePhotos(dir, renames) {
  const moved = [];
  for (const { from, to } of renames) {
    for (const e of ['.jpg', '.png']) {
      const src = path.join(dir, from + e);
      if (!fs.existsSync(src)) continue;
      fs.renameSync(src, path.join(dir, to + e));
      moved.push(`${from}${e}`);
    }
  }
  return moved;
}

module.exports = { COLUMNS, hashOf, wrap, unwrap, vault, check, apply, configs, renamePhotos };

if (require.main === module) {
  (async () => {
    const mode = process.argv[2];
    if (mode === 'configs') {
      const { Pool } = require('pg');
      const pool = new Pool({ connectionString: process.env.DATABASE_URL });
      try {
        const done = await configs({
          pool,
          oldVault: vault(process.env.OLD_PASSETTO_URL),
          newVault: vault(process.env.NEW_PASSETTO_URL),
        });
        console.log(`configs: ${done.values} encrypted values in ${done.rows} rows, re-encrypted and read back`);
      } catch (err) {
        console.error(`STOPPED: ${err.message}`);
        process.exitCode = 1;
      } finally {
        await pool.end();
      }
      return;
    }
    if (mode !== 'check' && mode !== 'apply') {
      console.error('usage: node rotate.js check|apply [--avatars DIR] | configs');
      process.exit(2);
    }
    const salt = (name) => fs.readFileSync(process.env[name], 'utf8').trim();
    const { Pool } = require('pg');
    const pool = new Pool({ connectionString: process.env.DATABASE_URL });
    const oldVault = vault(process.env.OLD_PASSETTO_URL);
    try {
      const result =
        mode === 'check'
          ? await check({ pool, oldVault, oldSalt: salt('OLD_SALT_FILE') })
          : await apply({
              pool,
              oldVault,
              newVault: vault(process.env.NEW_PASSETTO_URL),
              oldSalt: salt('OLD_SALT_FILE'),
              newSalt: salt('NEW_SALT_FILE'),
            });
      for (const [col, c] of result.counts) console.log(`${col}: ${c.values} values, ${c.hashes} hashes`);
      console.log(`${mode}: ${result.rows.length} values, every one read and checked`);
      console.log(`erased accounts' markers left as they are: ${result.erased.length}`);
      if (mode === 'apply') {
        const at = process.argv.indexOf('--avatars');
        if (at > 0) {
          const moved = renamePhotos(process.argv[at + 1], result.renames);
          console.log(`photographs renamed: ${moved.length} of ${result.renames.length} passengers have one`);
        } else {
          console.log(`photographs NOT renamed (no --avatars): ${result.renames.length} to check`);
        }
      }
    } catch (err) {
      console.error(`STOPPED: ${err.message}`);
      process.exitCode = 1;
    } finally {
      await pool.end();
    }
  })();
}
