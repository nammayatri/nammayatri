-- No need to run this migration, these are already there !! --
CREATE TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ();

ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN buffer_kms integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN buffer_meters integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN fare_policy_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN ride_duration integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD PRIMARY KEY ( fare_policy_id);

-- No need to run migrations till here, these are already there !! --