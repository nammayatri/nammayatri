-- Where a driver's 3 000 DA a month is recorded.
--
--   docker cp driver-subscription.sql ny-postgres:/tmp/ && \
--   docker exec ny-postgres psql -U postgres -d atlas_dev -f /tmp/driver-subscription.sql
--
-- Idempotent: every statement is IF NOT EXISTS, so running it twice is safe and
-- running it against a database that already has the tables changes nothing.
--
-- ── Why this is our own schema and not the backend's ───────────────────────
-- probe-subscription.sql measured it on 2026-08-16: there is nowhere in either
-- upstream schema to record a payment. No plan, fee, subscription, invoice,
-- mandate or order table, and none of those words in the binary either --
-- upstream's driver-subscription subsystem is simply not in this build. So the
-- choice is a rebuild or our own tables, and these are our own tables.
--
-- `movin` rather than a couple of loose tables inside `atlas_driver_offer_bpp`,
-- for the same reason `geo` holds the place index: the upstream binary owns its
-- schemas and runs its own migrations over them. Anything of ours that sits in
-- there is one upstream migration away from being dropped, and would also make
-- a `pg_dump` of their schema stop being a dump of their schema.
--
-- ── No foreign key to person(id), deliberately ─────────────────────────────
-- `driver_id` is `atlas_driver_offer_bpp.person.id` and an FK would express
-- that correctly. It is left off anyway: an FK from our schema into theirs
-- takes a lock on their table and can block one of their migrations, and the
-- failure would land on a backend deploy with nothing to say it came from here.
-- The cost of leaving it off is a possible orphan row after a driver is deleted,
-- which is a row about a payment that really happened.

BEGIN;

CREATE SCHEMA IF NOT EXISTS movin;

-- ── Who is paid up ─────────────────────────────────────────────────────────
--
-- One row per driver, written only by the webhook. `paid_until` NULL means he
-- has never paid -- which is NOT the same as lapsed, and the difference matters
-- on the day this ships: every driver in the pilot starts NULL, and treating
-- that as "unpaid, restrict him" would restrict the entire fleet at once. See
-- the README: what happens to the existing drivers is the office's decision and
-- has not been made.
CREATE TABLE IF NOT EXISTS movin.subscription (
  driver_id  text PRIMARY KEY,
  paid_until timestamptz,
  created_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz NOT NULL DEFAULT now()
);

-- ── Receipt numbers ────────────────────────────────────────────────────────
--
-- The client's answer on 2026-08-25 was "no need for receipts, but we should be
-- able to generate them". A receipt anyone would accept needs a number that
-- does not skip, so it is drawn when a payment is *applied* and never at
-- insert: a `bigserial` on the row would burn a number on every abandoned
-- checkout, and a receipt book that jumps from 14 to 31 is a receipt book with
-- seventeen missing receipts as far as anyone auditing it is concerned.
--
-- Sequences are exempt from rollback by design, so a number can still be lost
-- if the transaction that drew it aborts. That is a gap of one after a crash,
-- not a gap of one per abandoned payment.
CREATE SEQUENCE IF NOT EXISTS movin.invoice_seq START 1;

-- ── Every checkout we ever created ─────────────────────────────────────────
--
-- `checkout_id` is Chargily's own id and it is the PRIMARY KEY on purpose: it
-- is the idempotency key. Their webhooks are retried, so without a uniqueness
-- constraint plus the `applied_at IS NULL` guard in subscription.js, one retry
-- is a free month. The table is the guard; the code just leans on it.
--
-- `event` keeps the whole webhook body. It is the only evidence we will have if
-- Chargily and our figures ever disagree, and it costs nothing at this volume.
CREATE TABLE IF NOT EXISTS movin.subscription_payment (
  checkout_id  text PRIMARY KEY,
  driver_id    text NOT NULL,
  amount       integer NOT NULL,
  currency     text NOT NULL DEFAULT 'dzd',
  months       integer NOT NULL DEFAULT 1,
  -- pending -> paid | failed | canceled. Chargily's own vocabulary, minus the
  -- `checkout.` prefix on the event type.
  status       text NOT NULL DEFAULT 'pending',
  invoice_no   bigint UNIQUE,
  -- What the month he bought actually covers. Recorded rather than recomputed
  -- so a receipt reprinted next year still says what it said on the day.
  covers_from  timestamptz,
  covers_until timestamptz,
  paid_at      timestamptz,
  -- Set exactly once, by the webhook, inside the same transaction that extends
  -- the subscription. This is the replay guard.
  applied_at   timestamptz,
  created_at   timestamptz NOT NULL DEFAULT now(),
  event        jsonb
);

CREATE INDEX IF NOT EXISTS subscription_payment_driver
  ON movin.subscription_payment (driver_id, created_at DESC);

-- ── What the office will want to read ──────────────────────────────────────
--
-- Nobody has built an admin screen yet, and until somebody does this is how the
-- question "who owes us money" gets answered:
--
--   docker exec ny-postgres psql -U postgres -d atlas_dev \
--     -c "SELECT * FROM movin.driver_subscription_state ORDER BY state, name"
--
-- Left joins throughout: a driver with no subscription row must still appear,
-- because he is precisely the driver the office is looking for.
CREATE OR REPLACE VIEW movin.driver_subscription_state AS
SELECT p.id                              AS driver_id,
       trim(coalesce(p.first_name, '') || ' ' || coalesce(p.last_name, '')) AS name,
       p.unencrypted_mobile_number       AS phone,
       di.enabled,
       di.blocked,
       s.paid_until,
       CASE
         WHEN s.paid_until IS NULL      THEN 'never'
         WHEN s.paid_until > now()      THEN 'active'
         ELSE                                'lapsed'
       END                               AS state,
       CASE WHEN s.paid_until IS NULL THEN NULL
            ELSE floor(extract(epoch FROM s.paid_until - now()) / 86400)::int
       END                               AS days_left,
       (SELECT count(*) FROM movin.subscription_payment sp
         WHERE sp.driver_id = p.id AND sp.applied_at IS NOT NULL) AS months_paid
  FROM atlas_driver_offer_bpp.person p
  JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
  LEFT JOIN movin.subscription s ON s.driver_id = p.id
 WHERE p.role = 'DRIVER';

COMMIT;

\echo == movin schema ready. Current state of the fleet:
SELECT state, count(*) FROM movin.driver_subscription_state GROUP BY state ORDER BY state;
