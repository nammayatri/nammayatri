-- INSERT INTO atlas_driver_offer_bpp.merchant_service_config
-- VALUES ('favorit0-0000-0000-0000-00000favorit', 'Verification_InternalScripts', '{"url":"http://localhost:5000/"}');

--local sync
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id,merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'Verification_InternalScripts','{"url":"http://localhost:5000/"}'
FROM atlas_driver_offer_bpp.merchant_operating_city m where m.merchant_id = 'favorit0-0000-0000-0000-00000favorit';

-- INSERT INTO atlas_driver_offer_bpp.merchant_service_config
-- VALUES ('favorit0-0000-0000-0000-00000favorit', 'Verification_InternalScripts', '{"url":"http://localhost:5000/"}');

--UPDATE atlas_driver_offer_bpp.merchant_service_usage_config SET face_verification_service ='InternalScripts';