'use strict';
// A real Postgres for the shims' tests, in-process: PGlite (Postgres compiled
// to WebAssembly). No server, no Docker -- `npm ci` in tests/ is the whole setup.
//
// Why not the fake pools the older tests use (a regex per query): on the money
// path the SQL IS the rule. `day_until > now() OR balance >= price`, "credited
// exactly once", "one charge per ride" -- a fake that answers by pattern tests
// none of that. Here our own migrations (stack/db/*.sql) build the movin
// schema, and every query the shim sends runs for real.
//
//   const { open } = require('./lib/db');
//   const db = await open();            // movin schema + the upstream tables we read
//   db.pool                             // what the shim gets: pg's Pool, in shape
//   await db.sql('INSERT ...', [..])    // for the test's own setup and checks
const fs = require('fs');
const path = require('path');

const STACK_DB = path.join(__dirname, '..', '..', 'stack', 'db');

/**
 * Only the columns the shims read, of the upstream tables they read. The real
 * ones come from the Haskell apps' migrations; these are cut down, never
 * invented -- every name below is one a shim's query uses.
 */
const UPSTREAM = `
  CREATE SCHEMA atlas_driver_offer_bpp;
  CREATE TABLE atlas_driver_offer_bpp.person (
    id text PRIMARY KEY,
    merchant_id character(36) NOT NULL,
    role text NOT NULL DEFAULT 'DRIVER',
    unencrypted_mobile_number text
  );
  CREATE TABLE atlas_driver_offer_bpp.ride (
    id text PRIMARY KEY,
    driver_id text NOT NULL,
    status text NOT NULL,
    trip_start_time timestamptz,
    created_at timestamptz NOT NULL DEFAULT now()
  );
  CREATE TABLE atlas_driver_offer_bpp.driver_information (
    driver_id text PRIMARY KEY,
    on_ride boolean NOT NULL DEFAULT false
  );
  CREATE SCHEMA atlas_app;
  CREATE TABLE atlas_app.person (
    id text PRIMARY KEY,
    unencrypted_mobile_number text
  );
  CREATE TABLE atlas_app.booking (id text PRIMARY KEY, rider_id text NOT NULL);
  CREATE TABLE atlas_app.ride (
    id text PRIMARY KEY,
    booking_id text NOT NULL,
    status text NOT NULL
  );
`;

/** Our own migrations, exactly as the server applied them. */
const OURS = ['driver-wallet.sql', 'account-deletion.sql'];

let PGlite;
function load() {
  if (PGlite) return PGlite;
  try {
    ({ PGlite } = require('@electric-sql/pglite'));
  } catch {
    console.error('PGlite is missing -- run `npm ci` in Backend/dev/local-stack/tests first');
    process.exit(2);
  }
  return PGlite;
}

/** psql's own meta-commands are not SQL; the server ran these files with psql. */
const forPglite = (sql) => sql.split('\n').filter((l) => !/^\s*\\/.test(l)).join('\n');

async function open() {
  const P = load();
  // int8 comes back as a string, as it does from `pg`: invoice numbers and
  // count(*) must look here exactly as they look in production.
  const db = new P({ parsers: { 20: (v) => v } });
  await db.exec(UPSTREAM);
  for (const f of OURS) await db.exec(forPglite(fs.readFileSync(path.join(STACK_DB, f), 'utf8')));

  // PGlite is one connection. A shim's transaction (BEGIN ... COMMIT across
  // several awaits) must not interleave with another caller's statements, so
  // a client holds a lock until it is released -- what a pool checkout gives
  // you for real, made explicit.
  let chain = Promise.resolve();
  const exclusive = () => {
    let release;
    const held = new Promise((r) => { release = r; });
    const ready = chain.then(() => {});
    chain = chain.then(() => held);
    return ready.then(() => release);
  };
  const run = async (text, params) => {
    const r = await db.query(text, params);
    return { rows: r.rows, rowCount: r.affectedRows || r.rows.length };
  };
  const pool = {
    // A test makes the database fail: the next query (failNext), or the next
    // one whose SQL matches a pattern (failWhen).
    failNext: 0,
    failWhen: null,
    async query(text, params) {
      if (pool.failNext > 0) { pool.failNext -= 1; throw new Error('database unavailable (test)'); }
      if (pool.failWhen && pool.failWhen.test(text)) { pool.failWhen = null; throw new Error('database unavailable (test)'); }
      const release = await exclusive();
      try { return await run(text, params); } finally { release(); }
    },
    async connect() {
      const release = await exclusive();
      return { query: run, release };
    },
  };
  return {
    pool,
    sql: async (text, params) => (await db.query(text, params)).rows,
    close: () => db.close(),
  };
}

module.exports = { open };
