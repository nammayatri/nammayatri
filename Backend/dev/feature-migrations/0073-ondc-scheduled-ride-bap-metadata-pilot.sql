-- ONDC 2.1.0 scheduled-ride pilot: the flag moved from transporter_config to
-- bap_metadata (per BAP, scoped to a merchant + city). Nothing copies the old
-- value over, so set it on the pilot BAPs' rows here.
--
-- Values below are from master: transporter_config.enable_ondc_scheduled_ride_support
-- was true only for MSIL_PARTNER / Delhi, and these are the BAPs that went
-- through the pilot flow there (each already has a static_terms_url recorded,
-- which is only stored on the pilot path). Prod ids differ -- re-derive them there.
--
-- Run BEFORE deploying the release that reads the flag from bap_metadata.
UPDATE atlas_driver_offer_bpp.bap_metadata
   SET merchant_id = '840327a8-f17c-4d7c-8199-a583cfaadc5f', -- MSIL_PARTNER
       merchant_operating_city_id = 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e', -- Delhi
       enable_ondc_scheduled_ride_support = true
 WHERE id IN ('workbench.ondc.tech',
              'ondc-preprod.sequelstring.com/buyer/ride-hailing')
   AND domain = 'MOBILITY';

-- Then evict the cached copies (exact keys, no pattern match), or the row
-- without the new fields is served until it expires. The flag is read through
-- the per-city key, which also caches a miss (Nothing), so any request these
-- BAPs made in the pilot city before the UPDATE keeps the pilot off until it
-- is deleted:
--   DEL CachedQueries:BapMetadata:MOBILITY:sid-workbench.ondc.tech
--   DEL CachedQueries:BapMetadata:MOBILITY:sid-workbench.ondc.tech:mocid-db1c62db-01ba-4cbf-9cce-ede8d2e1361e
--   DEL CachedQueries:BapMetadata:MOBILITY:sid-ondc-preprod.sequelstring.com/buyer/ride-hailing
--   DEL CachedQueries:BapMetadata:MOBILITY:sid-ondc-preprod.sequelstring.com/buyer/ride-hailing:mocid-db1c62db-01ba-4cbf-9cce-ede8d2e1361e
