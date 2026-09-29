-- Move driver_license uniqueness from global to per-merchant so the same DL can
-- be onboarded under a driver in each merchant. `unique_number` exists as a
-- table constraint on some envs and a bare unique index on others, so try the
-- constraint drop first (cascades its index) then the bare-index drop.
-- Legacy rows with merchant_id NULL are stamped lazily by the app path
-- (findByDLNumber resolves via driver_id -> person.merchant_id on first hit).

CREATE UNIQUE INDEX CONCURRENTLY driver_license_unique_number_merchant
  ON atlas_driver_offer_bpp.driver_license (license_number_hash, merchant_id);

ALTER TABLE atlas_driver_offer_bpp.driver_license DROP CONSTRAINT IF EXISTS unique_number;
DROP INDEX IF EXISTS atlas_driver_offer_bpp.unique_number;
