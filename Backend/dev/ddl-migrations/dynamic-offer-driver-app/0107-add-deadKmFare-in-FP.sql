alter table atlas_driver_offer_bpp.fare_parameters add column dead_km_fare double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters
  ALTER COLUMN dead_km_fare SET DATA TYPE integer
  USING round(dead_km_fare);