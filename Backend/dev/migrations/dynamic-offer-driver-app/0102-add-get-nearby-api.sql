ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_near_by_search character varying(30);
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET get_near_by_search = 'Google';
