-- The driver's wallet, and the day it buys.
--
-- ── The model, from the client on 2026-09-06 ───────────────────────────────
-- Replaces the monthly subscription entirely. A driver loads credit -- as much
-- as he likes, never less than 30 MRU -- and nothing is taken until he starts
-- working. At his first ride of a day, 30 MRU comes off and he is covered for
-- 24 hours. Every ride inside that window is free.
--
-- Three consequences worth stating, because each removes something the old
-- design needed:
--
--   * There is no subscription and no renewal. Nobody is ever charged without
--     driving, so the whole "pay-then-extend, never auto-debit" apparatus that
--     Algeria forced on us has nothing left to guard against.
--   * A top-up is a plain one-off payment. Moosyl has a full subscriptions API
--     with automatic billing and we do not need any of it -- which is the
--     safer half of it to not use.
--   * The day is bought at the first ride, not at the top-up. A driver who
--     loads 300 MRU on Monday and does not drive until Friday has spent
--     nothing on Tuesday, Wednesday or Thursday.
--
-- ── Why a ledger and not just a balance ────────────────────────────────────
-- Because a driver will ask where his 30 went, and "the balance says 270" is
-- not an answer. Every credit and every debit is a row, the balance is their
-- sum, and `movin.wallet.balance` is a cache of that sum rather than the truth.
-- A wallet whose history cannot be printed is a wallet nobody trusts, and the
-- person it has to convince is the one whose money it is.
--
-- Idempotent: safe to re-run.

CREATE SCHEMA IF NOT EXISTS movin;

-- ── What he has ────────────────────────────────────────────────────────────
--
-- `balance` is in whole MRU. Not centimes: Moosyl quotes amounts as integers
-- and the price is 30, so there is nothing below the unit to represent and a
-- second scale would only be somewhere for the two to disagree.
--
-- `day_until` is when the current 24 hours run out, or NULL if he has not
-- started one. It is set from the ride's own start time rather than from the
-- moment we notice, so a driver who starts at 07:00 is covered to 07:00 the
-- next day even if the sweep that charged him ran at 07:04.
CREATE TABLE IF NOT EXISTS movin.wallet (
  driver_id  text PRIMARY KEY,
  balance    integer NOT NULL DEFAULT 0,
  day_until  timestamptz,
  created_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz NOT NULL DEFAULT now()
);

-- ── Every movement, in order ───────────────────────────────────────────────
--
-- `amount` is signed: a top-up is positive, a day is negative. The balance is
-- `sum(amount)` and nothing else, so the two cannot drift in a way this table
-- does not explain.
--
-- `ride_id` is set on a day charge and is UNIQUE-per-driver-per-day by the
-- index below rather than by a constraint on the column: the same ride must
-- never be charged twice, which is the failure a polling sweep makes if it
-- runs while a ride is still starting.
CREATE TABLE IF NOT EXISTS movin.wallet_entry (
  id         bigserial PRIMARY KEY,
  driver_id  text NOT NULL,
  -- topup | day | adjustment
  kind       text NOT NULL,
  amount     integer NOT NULL,
  -- The top-up this came from, for a credit. NULL on a day charge.
  topup_id   text,
  -- The ride that opened the day, for a debit. NULL on a credit.
  ride_id    text,
  -- What the day covers. Recorded rather than recomputed, so a receipt
  -- reprinted next year still says what it said on the day.
  day_from   timestamptz,
  day_until  timestamptz,
  note       text,
  created_at timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS wallet_entry_driver_idx
  ON movin.wallet_entry (driver_id, created_at DESC);

-- One charge per ride, ever. The sweep that applies these is at-least-once by
-- nature -- it can be restarted mid-pass -- so the guarantee has to be here and
-- not in the code that calls it.
CREATE UNIQUE INDEX IF NOT EXISTS wallet_entry_one_charge_per_ride
  ON movin.wallet_entry (ride_id) WHERE ride_id IS NOT NULL;

-- ── Money coming in ────────────────────────────────────────────────────────
--
-- One row per checkout we ever opened, abandoned ones included. Kept because
-- "he says he paid" is answered by this table and by nothing else.
--
-- `transaction_id` is ours and is what Moosyl echoes back; `payment_ref` is
-- theirs. Both are stored because a support question can arrive with either.
CREATE TABLE IF NOT EXISTS movin.wallet_topup (
  transaction_id text PRIMARY KEY,
  driver_id      text NOT NULL,
  amount         integer NOT NULL,
  currency       text NOT NULL DEFAULT 'MRU',
  -- pending -> paid | failed | expired | cancelled
  status         text NOT NULL DEFAULT 'pending',
  payment_ref    text,
  checkout_url   text,
  invoice_no     bigint UNIQUE,
  -- Set exactly once, in the same transaction that writes the credit entry.
  -- Its presence is what makes crediting twice impossible.
  credited_at    timestamptz,
  paid_at        timestamptz,
  created_at     timestamptz NOT NULL DEFAULT now(),
  updated_at     timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS wallet_topup_driver_idx
  ON movin.wallet_topup (driver_id, created_at DESC);

-- Receipt numbers, drawn when a payment is applied and never at insert -- a
-- bigserial on the row would burn a number on every abandoned checkout, and a
-- receipt book that jumps from 14 to 31 is one with seventeen missing receipts
-- as far as anyone auditing it is concerned. Carried over unchanged from the
-- subscription this replaces.
CREATE SEQUENCE IF NOT EXISTS movin.invoice_seq START 1;

-- ── The balance, derived ───────────────────────────────────────────────────
--
-- For checking the cache rather than for serving requests. If this and
-- `movin.wallet.balance` ever disagree, the ledger is right.
CREATE OR REPLACE VIEW movin.wallet_check AS
  SELECT w.driver_id,
         w.balance                          AS cached,
         COALESCE(SUM(e.amount), 0)::int    AS ledger,
         w.balance - COALESCE(SUM(e.amount), 0)::int AS drift
    FROM movin.wallet w
    LEFT JOIN movin.wallet_entry e ON e.driver_id = w.driver_id
   GROUP BY w.driver_id, w.balance;
