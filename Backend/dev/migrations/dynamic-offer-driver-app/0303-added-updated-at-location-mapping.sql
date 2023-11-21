ALTER TABLE atlas_driver_offer_bpp.location_mapping ADD COLUMN created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.location_mapping ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.location_mapping ADD COLUMN merchant_id character(36) REFERENCES atlas_driver_offer_bpp.merchant (id);
ALTER TABLE atlas_driver_offer_bpp.location_mapping ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);


-- Run the below migration in prod (for backfilling)
-- UPDATE atlas_driver_offer_bpp.location_mapping lm
-- SET merchant_id = sr.provider_id,
-- merchant_operating_city_id = sr.merchant_operating_city_id
-- FROM atlas_driver_offer_bpp.search_request sr
-- WHERE (lm.merchant_id IS NULL OR lm.merchant_operating_city_id IS NULL)
--   AND lm.tag = 'SEARCH_REQUEST'
--   AND lm.entity_id = sr.id;

-- UPDATE atlas_driver_offer_bpp.location_mapping lm
-- SET created_at = sr.created_at FROM atlas_driver_offer_bpp.search_request sr
-- WHERE lm.tag = 'SEARCH_REQUEST'
--   AND lm.entity_id = sr.id;
