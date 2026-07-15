-- Backfill the special_location table's new payment_modes text[] column with the default
-- ['CASH'] for existing rows, in both schemas. The column is added by the pure-DDL
-- migrations ddl-migrations/rider-app/1544 and ddl-migrations/dynamic-offer-driver-app/0835.

UPDATE atlas_app.special_location
SET payment_modes = ARRAY['CASH']
WHERE payment_modes IS NULL;

UPDATE atlas_driver_offer_bpp.special_location
SET payment_modes = ARRAY['CASH']
WHERE payment_modes IS NULL;
