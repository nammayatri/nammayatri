INSERT INTO atlas_driver_offer_bpp.special_location_priority (id, merchant_id, category, pickup_priority, drop_priority)
  SELECT
      md5(random()::text || clock_timestamp()::text)::uuid,
      T1.id,
      'SureShoppingMall',
      2,
      3
  FROM atlas_driver_offer_bpp.merchant AS T1;
INSERT INTO atlas_driver_offer_bpp.special_location_priority (id, merchant_id, category, pickup_priority, drop_priority)
  SELECT
      md5(random()::text || clock_timestamp()::text)::uuid,
      T1.id,
      'UnSureShoppingMall',
      2,
      3
  FROM atlas_driver_offer_bpp.merchant AS T1;