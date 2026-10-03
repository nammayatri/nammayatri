-- No need to run this migration, these are already there !! --
CREATE TABLE atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs ();

ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs ADD COLUMN distance_percentage integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs ADD COLUMN fare_percentage integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs ADD COLUMN fare_policy_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs ADD COLUMN include_actual_dist_percentage boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs ADD COLUMN include_actual_time_percentage boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs ADD COLUMN time_percentage integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs ADD PRIMARY KEY ( distance_percentage, fare_policy_id, time_percentage);

-- No need to run migrations till here, these are already there !! --