CREATE TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ();

ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD COLUMN cgst_percentage double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD COLUMN max_distance_meters integer ;
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD COLUMN min_distance_meters integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD COLUMN platform_fee double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD COLUMN sgst_percentage double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD PRIMARY KEY ( merchant_operating_city_id, min_distance_meters);

-- Bound sanity checks only -- NOT an overlap guard. Preventing overlapping ranges across rows
-- needs a real Postgres EXCLUDE constraint on a range type (btree_gist), which is out of scope
-- here; overlap is instead made harmless at read time by Storage.CachedQueries.
-- IntercityPlatformFeeSlab sorting ascending by min_distance_meters before selection.
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD CONSTRAINT intercity_platform_fee_slab_non_negative_min_distance CHECK (min_distance_meters >= 0);
ALTER TABLE atlas_driver_offer_bpp.intercity_platform_fee_slab ADD CONSTRAINT intercity_platform_fee_slab_max_gt_min_distance CHECK (max_distance_meters IS NULL OR max_distance_meters > min_distance_meters);
