-- The driver's monthly subscription, retired (phase 6, 2026-10-07).
--
-- Applied once, by the release that ships it (ops/deploy.sh applies a db/*.sql
-- that is new to the server). Idempotent: on a database that never had the
-- tables -- a fresh dev stack, CI -- every statement is a no-op.
--
-- ── What goes ───────────────────────────────────────────────────────────────
-- The three objects `driver-subscription.sql` made on 2026-08-26 for 3 000 DA a
-- month through Chargily: `movin.subscription` (33 rows, last written
-- 2026-08-26), `movin.subscription_payment` (9 rows, 1 applied, last
-- 2026-08-28) and the view over them, `movin.driver_subscription_state`. The
-- wallet replaced them on 2026-09-07; no phone had called /subscription/ since
-- 2026-09-02, and the routes were retired the release before this one.
--
-- ── What stays ──────────────────────────────────────────────────────────────
-- `movin.invoice_seq`. The subscription created it, but the wallet's top-ups
-- draw their receipt numbers from it too (`driver-wallet.sql` creates it as
-- well), and restarting it would hand out receipt numbers already issued.
--
-- ── The way back ────────────────────────────────────────────────────────────
-- A dump of exactly these three objects, taken just before this ran and kept
-- with the backups, encrypted with the same passphrase: the file
-- `subscription-final-<UTC time>.sql.gpg`, in the backup directory and offsite.
-- Restoring it recreates them with their rows.
--
-- ── The guard ───────────────────────────────────────────────────────────────
-- If anything was written to them after 2026-09-01, something still uses them
-- and the measurement this rests on is wrong: stop, drop nothing.

BEGIN;

-- Nested IFs and EXECUTE, not `IF exists AND EXISTS (SELECT ...)`: PL/pgSQL
-- plans the whole condition, so a database without the table fails on the
-- name before AND gets a chance to short-circuit (caught testing this file
-- against a fresh database).
DO $$
DECLARE late boolean;
BEGIN
  IF to_regclass('movin.subscription') IS NOT NULL THEN
    EXECUTE 'SELECT EXISTS (SELECT 1 FROM movin.subscription
                             WHERE greatest(created_at, updated_at) > ''2026-09-01'')' INTO late;
    IF late THEN
      RAISE EXCEPTION 'movin.subscription was written after 2026-09-01: something still uses it, nothing dropped';
    END IF;
  END IF;
  IF to_regclass('movin.subscription_payment') IS NOT NULL THEN
    EXECUTE 'SELECT EXISTS (SELECT 1 FROM movin.subscription_payment
                             WHERE created_at > ''2026-09-01'')' INTO late;
    IF late THEN
      RAISE EXCEPTION 'movin.subscription_payment was written after 2026-09-01: something still uses it, nothing dropped';
    END IF;
  END IF;
END $$;

DROP VIEW IF EXISTS movin.driver_subscription_state;
DROP TABLE IF EXISTS movin.subscription_payment;
DROP TABLE IF EXISTS movin.subscription;

COMMIT;
