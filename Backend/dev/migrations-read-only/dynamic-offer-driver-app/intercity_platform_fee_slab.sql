CREATE TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ();

ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD COLUMN max_distance_meters integer ;
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD COLUMN min_distance_meters integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD COLUMN platform_fee double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD PRIMARY KEY ( merchant_operating_city_id, min_distance_meters);
