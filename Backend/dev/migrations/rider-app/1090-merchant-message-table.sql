WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'INVITE_TO_UNEXISTENT_EMERGENCY_NUMBER', 'You have been added as an Emergency Contact by {#customerNumber#} for their Namma Yatri auto-rickshaw trips.  For you to track their trips, please download and signup in NammaYatri: <link>.', T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'SET_AS_RIDE_EMERGENCY_NUMBER', 'Customer {#whoSetMobileNumber#} set you as emergency number for his ride. Check the app for details.', T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'SET_AS_DEFAULT_EMERGENCY_NUMBER', 'Customer {#whoSetMobileNumber#} set you as his default emergency number.', T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'SEND_OTP', '<#> Your OTP for login to Yatri App is {#otp#} {#hash#}', T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);