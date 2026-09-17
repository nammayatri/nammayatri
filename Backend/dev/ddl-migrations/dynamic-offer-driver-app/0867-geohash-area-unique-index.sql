-- One label per (city, geohash) -- lets the dashboard bulk-upsert endpoint update instead of duplicate.
ALTER TABLE atlas_driver_offer_bpp.geohash_area ADD CONSTRAINT geohash_area_unique_idx_merchant_operating_city_id_geohash UNIQUE (merchant_operating_city_id, geohash);
