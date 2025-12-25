
--ONLY FOR LOCAL SYNC
ALTER TABLE atlas_driver_offer_bpp.merchant_message DROP CONSTRAINT merchant_message_pkey;
ALTER TABLE atlas_driver_offer_bpp.merchant_message ALTER COLUMN merchant_operating_city_id DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_message ADD PRIMARY KEY (merchant_id, message_key);

WITH MerchantMessages AS (
  SELECT T1.id, 'SEND_OTP', '<#> Your OTP for login to Yatri App is {#otp#} {#hash#}'
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.id, 'WELCOME_TO_PLATFORM', 'Welcome to the Yatri platform! Your agency ({#orgName#}) has added you as a driver. Start getting rides by installing the app: {#link#}'
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message)
  (SELECT * FROM MerchantMessages);