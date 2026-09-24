-- No need to run this migration, these are already there !! --
CREATE TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section ();

ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section ADD COLUMN base_fare_depreciation double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section ADD COLUMN fare_policy_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section ADD COLUMN per_extra_km_rate double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section ADD COLUMN start_distance integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section ADD PRIMARY KEY ( fare_policy_id);

-- No need to run migrations till here, these are already there !! --