-- Remove duplicate seed rows, then make them impossible.
--
-- ── What goes wrong without this ────────────────────────────────────────────
-- The upstream baseline creates fare_policy with no primary key and no unique
-- constraint at all (migration 0016-fare-policy.sql builds fare_policy_new with
-- only a foreign key). Nothing stops the same row being inserted twice.
--
-- And something does insert it twice. `seed_driver_db` loads
-- local-testing-data/dynamic-offer-driver-app.sql with `|| true` and output
-- suppressed, so a second run of setup.sh quietly adds the two organisation
-- rows again. Migration 0016 then cross-joins whatever it finds against four
-- vehicle variants:
--
--     INSERT INTO fare_policy_new
--     SELECT ..., fp.organization_id, v.variant, ...
--       FROM fare_policy fp, tmp_variants v;
--
-- so 2 organisation rows become 8 fare policies, and 4 become 16.
--
-- ── Why 16 is worse than wrong ──────────────────────────────────────────────
-- Ride search tolerates it and simply returns each vehicle twice: eight
-- estimates where there should be four, all at the same price. That looks like
-- a richer result rather than a fault, and it is what the stack has been
-- returning since it was deployed.
--
-- Booking then fails at the first step. The BPP's /select handler looks the fare
-- policy up expecting exactly one row and gets two:
--
--     E500 INTERNAL_ERROR: PersistError "Multiple results of Entity FarePolicyT"
--
-- which reaches the rider app as a bare 500 BECKN_API_CALL_ERROR on select. No
-- ride could ever be booked, and nothing before select showed a symptom.
--
-- The unique indexes below are the actual fix: the deletes clean up an existing
-- database, but the constraint is what stops the next re-run recreating it.
--
-- Idempotent: safe to re-run.

BEGIN;

-- ── fare_policy ─────────────────────────────────────────────────────────────
-- Keep the lowest id per (merchant, variant). The duplicates are byte-identical
-- apart from the id -- they share a created_at to the microsecond, being copies
-- of one row made by one cross join -- so which survives does not matter.
DELETE FROM atlas_driver_offer_bpp.fare_policy f
 WHERE f.id <> (
       SELECT min(g.id)
         FROM atlas_driver_offer_bpp.fare_policy g
        WHERE g.merchant_id     = f.merchant_id
          AND g.vehicle_variant = f.vehicle_variant);

CREATE UNIQUE INDEX IF NOT EXISTS fare_policy_merchant_variant_uniq
    ON atlas_driver_offer_bpp.fare_policy (merchant_id, vehicle_variant);

-- ── merchant_service_config ─────────────────────────────────────────────────
-- This one is a guard, not a fix. Listing the table shows every service name
-- twice (Maps_Google, Maps_OSRM, the SMS providers) which looks like the same
-- duplication -- it is not. There are two merchants, and each holds its own row
-- per service. The DELETE below finds nothing on a healthy database, and the
-- unique index proves it: a genuinely duplicated table could not build one.
--
-- It is kept because this table has no constraint either, and it is loaded by
-- the same re-runnable seed. Cheap insurance against the same failure mode.
--
-- No id column here, so identity is the physical row.
DELETE FROM atlas_driver_offer_bpp.merchant_service_config c
 WHERE c.ctid <> (
       SELECT min(d.ctid)
         FROM atlas_driver_offer_bpp.merchant_service_config d
        WHERE d.merchant_id  = c.merchant_id
          AND d.service_name = c.service_name);

CREATE UNIQUE INDEX IF NOT EXISTS msc_merchant_service_uniq
    ON atlas_driver_offer_bpp.merchant_service_config (merchant_id, service_name);

COMMIT;
