-- Fleet config is looked up by (fleet_owner_id, merchant_operating_city_id); backfill the city for older rows.
UPDATE atlas_driver_offer_bpp.fleet_config fc
   SET merchant_operating_city_id = p.merchant_operating_city_id,
       merchant_id = COALESCE(fc.merchant_id, p.merchant_id)
  FROM atlas_driver_offer_bpp.person p
 WHERE p.id = fc.fleet_owner_id
   AND fc.merchant_operating_city_id IS NULL;
