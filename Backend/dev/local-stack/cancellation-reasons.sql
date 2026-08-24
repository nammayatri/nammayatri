-- Give the driver app a list of cancellation reasons to read.
--
-- ── The gap this closes ─────────────────────────────────────────────────────
-- `GET /ui/cancellationReason/list` is published, the table has exactly the
-- right columns, and it has **never had a row**. Measured 2026-08-23: the route
-- answers `[]` with a valid driver token. The passenger side's equivalent table
-- carries the seven English rows of the upstream seed; the driver's is empty.
--
-- So the app carries five reasons of its own. That works and costs two things:
-- changing the list needs a new APK on every phone, and nothing guarantees the
-- codes sent match what the agency reads —`CancellationReasonCode` is a bare
-- string with no enum, and the server stores whatever it is given.
--
-- ── Why it matters more than it looks ───────────────────────────────────────
-- Thirteen cancellations in this database: nine by passengers with a NULL
-- reason, four by drivers all saying `OTHER`. Zero usable information on
-- thirteen failed rides, and a cancellation sends the passenger back to a fresh
-- search rather than to the next driver — with `driverBatchSize = 5` and one
-- batch in this deployment, that is five drivers, minus the one who just
-- cancelled.
--
-- ── The wording is a product decision, not a technical one ──────────────────
-- Once these are in the table they become the vocabulary of every report the
-- agency will ever run, and changing them later cuts the history in two. They
-- are the five already shipped in the app plus `TOO_FAR`, which is the only
-- extra the measured distance to the passenger makes plausible: 1 588 m on
-- average, up to 4 391 m.
--
-- `priority` orders the list as the driver sees it. `ADDRESS_NOT_FOUND` sits
-- second because it is the commonest thing that goes wrong at night.
--
-- Idempotent: deletes exactly these codes before inserting, so re-running is
-- safe and does not depend on a unique constraint existing.
--
-- Apply with:  ./apply-migration.sh cancellation-reasons.sql
--
-- APPLIED 2026-08-24. The measurement above ("answers []") was true until that
-- afternoon and is kept as the record of why this file exists. Re-running it is
-- safe: the DELETE names exactly these six codes, so it replaces its own rows
-- and touches nothing else.

\set ON_ERROR_STOP on

BEGIN;

DELETE FROM atlas_driver_offer_bpp.cancellation_reason
 WHERE reason_code IN ('PASSENGER_NO_SHOW', 'ADDRESS_NOT_FOUND',
                       'PASSENGER_CANCELLED', 'VEHICLE_PROBLEM',
                       'TOO_FAR', 'OTHER');

INSERT INTO atlas_driver_offer_bpp.cancellation_reason
  (reason_code, description, enabled, priority)
VALUES
  ('PASSENGER_NO_SHOW',   'Le passager n''est pas venu',    true, 1),
  ('ADDRESS_NOT_FOUND',   'Adresse introuvable',            true, 2),
  ('PASSENGER_CANCELLED', 'Le passager a annulé sur place', true, 3),
  ('VEHICLE_PROBLEM',     'Problème de véhicule',           true, 4),
  ('TOO_FAR',             'Le passager est trop loin',      true, 5),
  ('OTHER',               'Autre',                          true, 9);

COMMIT;

-- What the driver app will now be served. `enabled = false` hides a reason
-- without losing the rows already recorded against it.
SELECT priority, reason_code, description, enabled
  FROM atlas_driver_offer_bpp.cancellation_reason
 ORDER BY priority;
