ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS negative_fare_adjustment integer, ADD COLUMN IF NOT EXISTS negative_fare_adjustment_amount double precision;
