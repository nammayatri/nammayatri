ALTER TABLE atlas_app.merchant ADD COLUMN minimum_driver_rates_count int;
UPDATE atlas_app.merchant SET minimum_driver_rates_count = 5
