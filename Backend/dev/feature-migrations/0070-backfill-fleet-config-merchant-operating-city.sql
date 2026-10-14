-- Backfill fleet_config.merchant_id / merchant_operating_city_id from the fleet owner. NO DDL
-- here; the columns already exist (NammaDSL-generated fleet_config.sql).
--
-- Fleet config is now read through Config Pilot, which loads a city's rows with
-- findAllByMerchantOperatingCityId. A row whose merchant_operating_city_id is NULL is never
-- loaded, so that fleet would silently fall back to the default Fleet Features. New rows and
-- dashboard updates set the city (upsertFleetConfig); this covers rows written before that.
UPDATE atlas_driver_offer_bpp.fleet_config fc
   SET merchant_operating_city_id = p.merchant_operating_city_id,
       merchant_id = COALESCE(fc.merchant_id, p.merchant_id)
  FROM atlas_driver_offer_bpp.person p
 WHERE p.id = fc.fleet_owner_id
   AND fc.merchant_operating_city_id IS NULL;
