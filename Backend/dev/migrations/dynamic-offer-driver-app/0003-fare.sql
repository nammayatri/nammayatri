ALTER TABLE atlas_driver_offer_bpp.fare_policy RENAME COLUMN base_fare TO fare_for_pickup;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD CHECK (fare_for_pickup > 0);
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN fare_per_km double precision NOT NULL CHECK (fare_per_km > 0) DEFAULT 12;
DROP TABLE atlas_driver_offer_bpp.fare_policy_per_extra_km_rate;
