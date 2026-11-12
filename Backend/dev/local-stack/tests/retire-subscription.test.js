'use strict';
// db/retire-subscription.sql, in a real Postgres (PGlite). Phase 6, 2026-10-07.
//
// A release applies a db/*.sql the moment it is new to the server, so this
// file runs unattended against the live database. What it must do there, and
// on every other database it may meet:
//   - nothing at all where the tables never existed (a fresh dev stack, CI);
//   - refuse, dropping nothing, if either table was written after 2026-09-01;
//   - drop exactly the three objects, and keep movin.invoice_seq, which the
//     wallet's receipts still number from;
//   - run twice without complaint.
const assert = require('assert');
const fs = require('fs');
const path = require('path');

const SQL = fs.readFileSync(path.join(__dirname, '..', 'stack', 'db', 'retire-subscription.sql'), 'utf8');

(async () => {
  const { PGlite } = await import('@electric-sql/pglite');
  const db = new PGlite();
  const apply = async () => {
    try {
      await db.exec(SQL);
      return 'ok';
    } catch (e) {
      await db.exec('ROLLBACK').catch(() => {});
      return e.message;
    }
  };
  const objects = async () => (await db.query(
    `SELECT c.relname FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
      WHERE n.nspname = 'movin' AND c.relkind IN ('r', 'v', 'S') ORDER BY 1`,
  )).rows.map((r) => r.relname);
  let n = 0;
  const check = (what, ok) => { assert.ok(ok, what); n += 1; console.log(`  ok  ${what}`); };

  check('no movin schema at all: a no-op', (await apply()) === 'ok');

  // The shapes the live tables have, as far as the script reads them.
  await db.exec(`
    CREATE SCHEMA movin;
    CREATE SEQUENCE movin.invoice_seq;
    CREATE TABLE movin.wallet (driver_id text PRIMARY KEY);
    CREATE TABLE movin.subscription (
      driver_id text PRIMARY KEY, paid_until timestamptz,
      created_at timestamptz NOT NULL DEFAULT now(), updated_at timestamptz NOT NULL DEFAULT now());
    CREATE TABLE movin.subscription_payment (
      checkout_id text PRIMARY KEY, driver_id text,
      created_at timestamptz NOT NULL DEFAULT now(), applied_at timestamptz);
    CREATE VIEW movin.driver_subscription_state AS
      SELECT s.driver_id, s.paid_until,
             (SELECT count(*) FROM movin.subscription_payment p WHERE p.driver_id = s.driver_id) AS payments
        FROM movin.subscription s;
    INSERT INTO movin.subscription VALUES ('d1', '2026-09-25', '2026-08-26', '2026-08-26');
    INSERT INTO movin.subscription_payment VALUES ('c1', 'd1', '2026-08-28', '2026-08-28');
  `);
  const all = await objects();

  await db.exec(`UPDATE movin.subscription SET updated_at = '2026-10-01'`);
  check('a late write to subscription: refused', /subscription was written after/.test(await apply()));
  check('  ...and nothing dropped', JSON.stringify(await objects()) === JSON.stringify(all));

  await db.exec(`UPDATE movin.subscription SET updated_at = '2026-08-26';
                 UPDATE movin.subscription_payment SET created_at = '2026-09-30'`);
  check('a late payment row: refused', /subscription_payment was written after/.test(await apply()));
  check('  ...and nothing dropped', JSON.stringify(await objects()) === JSON.stringify(all));

  await db.exec(`UPDATE movin.subscription_payment SET created_at = '2026-08-28'`);
  check('the real case: applied', (await apply()) === 'ok');
  check('exactly the three objects gone; invoice_seq and wallet kept',
    JSON.stringify(await objects()) === JSON.stringify(['invoice_seq', 'wallet']));
  check('applied again: a no-op', (await apply()) === 'ok');

  console.log(`\nALL PASSED -- ${n} checks`);
})().catch((e) => { console.error(e.message || e); process.exit(1); });
