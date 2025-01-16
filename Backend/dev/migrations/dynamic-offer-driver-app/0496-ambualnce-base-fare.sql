ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN base_distance int NOT NULL Default 5000; -- value?

UPDATE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab set base_fare = per_km_rate * 5;
