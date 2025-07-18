-- IMPORTANT: Run in Both Master & Prod Before Release
UPDATE atlas_app.integrated_bpp_config SET config_json = jsonb_set(config_json::jsonb, '{contents}', '{}'::jsonb) WHERE config_json->>'tag' = 'ONDC';

-- Updating Feed Info in Integrated BPP Config
-- Note: This Mapping Migration is Important and Maybe Changed while things go to Prod, please note having latest mapping while Prod Release
-- WITH multimodal_feed AS (
--   SELECT
--     i.id,
--     g.feed_id,
--     m.city,
--     g.vehicle_type
--   FROM atlas_app.integrated_bpp_config AS i
--   INNER JOIN atlas_app.gtfs_feed_info AS g
--     ON i.merchant_operating_city_id = g.merchant_operating_city_id
--     AND i.vehicle_category = g.vehicle_type
--     AND i.merchant_id = g.merchant_id
--   INNER JOIN atlas_app.merchant_operating_city AS m
--     ON m.id = g.merchant_operating_city_id
--   WHERE i.platform_type = 'MULTIMODAL'
-- )
-- UPDATE atlas_app.integrated_bpp_config AS i
-- SET
--   feed_key = mf.feed_id,
--   agency_key = CASE
--     WHEN mf.city = 'Chennai' AND mf.vehicle_type = 'BUS' THEN mf.feed_id || ':CUMTA'
--     WHEN mf.city = 'Chennai' AND mf.vehicle_type = 'SUBWAY' THEN mf.feed_id || ':SR'
--     WHEN mf.city = 'Chennai' AND mf.vehicle_type = 'METRO' THEN mf.feed_id || ':CMRL'
--     WHEN mf.city = 'Bangalore' AND mf.vehicle_type = 'METRO' THEN mf.feed_id || ':BMRCL'
--     WHEN mf.city = 'Bangalore' AND mf.vehicle_type = 'BUS' THEN mf.feed_id || ':BMTC'
--     WHEN mf.city = 'Bhubaneshwar' AND mf.vehicle_type = 'BUS' THEN mf.feed_id || ':CRUT'
--     ELSE i.agency_key
--   END
-- FROM multimodal_feed AS mf
-- WHERE i.id = mf.id;

-- IN PROD: Backfill FRFS Search, Booking and Quote table IntegratedBPPConfigId (Also Disable From KV)
-- UPDATE atlas_app.frfs_search AS f
-- SET integrated_bpp_config_id = i.id
-- FROM atlas_app.integrated_bpp_config AS i
-- WHERE f.integrated_bpp_config_id IS NULL
--   AND f.partner_org_transaction_id IS NOT NULL
--   AND f.merchant_operating_city_id = i.merchant_operating_city_id
--   AND f.vehicle_type = i.vehicle_category
--   AND i.platform_type = 'PARTNERORG';

-- UPDATE atlas_app.frfs_search AS f
-- SET integrated_bpp_config_id = i.id
-- FROM atlas_app.integrated_bpp_config AS i
-- WHERE f.integrated_bpp_config_id IS NULL
--   AND f.merchant_operating_city_id = i.merchant_operating_city_id
--   AND f.vehicle_type = i.vehicle_category
--   AND i.platform_type = 'APPLICATION';

-- ALTER TABLE atlas_app.frfs_search ALTER COLUMN integrated_bpp_config_id SET NOT NULL;

-- UPDATE atlas_app.frfs_quote AS f
-- SET integrated_bpp_config_id = i.id
-- FROM atlas_app.integrated_bpp_config AS i
-- WHERE f.integrated_bpp_config_id IS NULL
--   AND f.partner_org_transaction_id IS NOT NULL
--   AND f.merchant_operating_city_id = i.merchant_operating_city_id
--   AND f.vehicle_type = i.vehicle_category
--   AND i.platform_type = 'PARTNERORG';

-- UPDATE atlas_app.frfs_quote AS f
-- SET integrated_bpp_config_id = i.id
-- FROM atlas_app.integrated_bpp_config AS i
-- WHERE f.integrated_bpp_config_id IS NULL
--   AND f.merchant_operating_city_id = i.merchant_operating_city_id
--   AND f.vehicle_type = i.vehicle_category
--   AND i.platform_type = 'APPLICATION';

-- ALTER TABLE atlas_app.frfs_quote ALTER COLUMN integrated_bpp_config_id SET NOT NULL;

-- UPDATE atlas_app.frfs_ticket_booking AS f
-- SET integrated_bpp_config_id = i.id
-- FROM atlas_app.integrated_bpp_config AS i
-- WHERE f.integrated_bpp_config_id IS NULL
--   AND f.partner_org_transaction_id IS NOT NULL
--   AND f.merchant_operating_city_id = i.merchant_operating_city_id
--   AND f.vehicle_type = i.vehicle_category
--   AND i.platform_type = 'PARTNERORG';

-- UPDATE atlas_app.frfs_ticket_booking AS f
-- SET integrated_bpp_config_id = i.id
-- FROM atlas_app.integrated_bpp_config AS i
-- WHERE f.integrated_bpp_config_id IS NULL
--   AND f.merchant_operating_city_id = i.merchant_operating_city_id
--   AND f.vehicle_type = i.vehicle_category
--   AND i.platform_type = 'APPLICATION';

-- ALTER TABLE atlas_app.frfs_ticket_booking ALTER COLUMN integrated_bpp_config_id SET NOT NULL;

-----------------------------------------------------------------------------------------------------
---- NOTE: Run this Query After Prod Release of Multimodal, Rider App and Metro FRFS for Cleanup ----
-----------------------------------------------------------------------------------------------------
-- DROP TABLE atlas_app.gtfs_feed_info; -- For Master & Prod, Post Prod Release
DROP TABLE atlas_app.station;
DROP TABLE atlas_app.route;
DROP TABLE atlas_app.route_stop_mapping;
