-- Give driver_offer somewhere to keep the car the offer is for.
--
-- ── What this is one half of ────────────────────────────────────────────────
-- The client asked, repeatedly, that a passenger choosing between drivers see
-- which car is actually coming — not after he has booked, but while he is
-- deciding. Nothing in the offer carried it: `DriverQuote` on the provider side
-- has a `vehicleVariant` and no model, and the rider's `DriverOffer` had
-- driverName, rating, distance, duration and validTill.
--
-- Two builds close that. The first taught the provider to write
-- "Renault|Clio|Grey" into the on_select item descriptor — a field upstream
-- sets to "" and never reads, chosen precisely so the shared BECKN types stay
-- untouched and the gateway and registry binaries cannot fall out of step. The
-- second teaches the rider to keep it instead of dropping it on the floor.
-- This column is where the rider keeps it.
--
-- ── Why a column and not a spare field ──────────────────────────────────────
-- `driver_name` was the tempting place: no migration, and narrowly safe —
-- `ride.driver_name` is written from on_update's fulfillment.agent.name, a
-- different path entirely, so a composite here would never reach the ride
-- screen. Rejected anyway. A driver_name reading "Ahmed|Renault|Clio|Grey" is a
-- trap for whoever next opens this table, and hidden encodings have already
-- cost this project time more than once.
--
-- ── Apply this BEFORE swapping the image ────────────────────────────────────
-- The order matters and it is the safe order, not the risky one:
--
--   1. run this. The currently deployed binary does not know the column and
--      does not care — Postgres fills NULL for a nullable column it is never
--      told about, and every insert keeps working.
--   2. swap to the new image.
--
-- Rollback is therefore a plain image swap with nothing to undo here: the old
-- binary tolerates the extra column exactly as it did in step 1. Do not drop it
-- on rollback — dropping it is the only way to turn a reversible deploy into an
-- irreversible one.
--
-- Idempotent: safe to re-run, and safe to run before or after the deploy.

\set ON_ERROR_STOP on

-- "make|model|colour", pipe-separated rather than JSON so a parse on the far
-- side cannot throw: worst case a field is empty and the passenger reads one
-- word less. NULL is the ordinary case and must stay legal — an older provider,
-- or a driver the office has not attached a vehicle to yet, and neither is a
-- reason to fail an offer that is otherwise good.
ALTER TABLE atlas_app.driver_offer
  ADD COLUMN IF NOT EXISTS vehicle_desc character varying(255);

COMMENT ON COLUMN atlas_app.driver_offer.vehicle_desc IS
  'Algeria: make|model|colour, from the on_select item descriptor. NULL when the provider sent none.';

-- What the deploy should look like afterwards. Nothing is written until an
-- on_select arrives from the new provider image, so an all-NULL column here is
-- the correct state on the day of the migration, not a failure.
SELECT
  count(*)                                   AS offers,
  count(vehicle_desc)                        AS with_vehicle,
  count(*) - count(vehicle_desc)             AS without
FROM atlas_app.driver_offer;
