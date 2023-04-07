ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_near_by_search character varying(30);
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET get_near_by_search = 'Google';
ALTER TABLE atlas_driver_offer_bpp.search_request_location ADD COLUMN is_near_by_search_a_p_i_used boolean DEFAULT false NOT NULL;
