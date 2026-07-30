-- Backfill merchant_id / merchant_operating_city_id on fleet_driver_association from the driver's
-- person record. The columns are added by the pure-DDL migration
-- ddl-migrations/dynamic-offer-driver-app/0846-add-merchant-city-to-fleet-driver-association.sql.
-- New rows already get these populated by the app; this fills the historical rows so the
-- fleet-driver listing query's NULL-tolerant merchant/city predicate can tighten to an exact
-- match. Guarded on "merchant_id IS NULL" so it only touches not-yet-backfilled rows and is safe
-- to re-run. Batch/replay as needed for very large tables.
UPDATE atlas_driver_offer_bpp.fleet_driver_association AS fda
SET merchant_id = p.merchant_id,
    merchant_operating_city_id = p.merchant_operating_city_id
FROM atlas_driver_offer_bpp.person AS p
WHERE fda.driver_id = p.id
  AND fda.merchant_id IS NULL;
