
------------ drop existing unique constraints and add new unique constraints to overlay and merchant_message tables ------
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD CONSTRAINT unique_on_category_name UNIQUE (vehicle_category, merchant_operating_city_id, language, overlay_key, udf1);

ALTER TABLE atlas_driver_offer_bpp.merchant_message ADD CONSTRAINT unique_on_merchant_id_message_key UNIQUE (merchant_operating_city_id, message_key, vehicle_category);