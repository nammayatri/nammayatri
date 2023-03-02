CREATE TABLE atlas_app.merchant_message (
    merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id),
    message_key character varying(255) NOT NULL,
    message character varying(255) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (merchant_id, message_key)
);

WITH MerchantMessages AS (
  SELECT T1.id, 'INVITE_TO_UNEXISTENT_EMERGENCY_NUMBER', 'You have been added as an Emergency Contact by {#customerNumber#} for their Namma Yatri auto-rickshaw trips.  For you to track their trips, please download and signup in NammaYatri: <link>.'
  FROM atlas_app.merchant AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.id, 'SET_AS_RIDE_EMERGENCY_NUMBER', 'Customer {#whoSetMobileNumber#} set you as emergency number for his ride. Check the app for details.'
  FROM atlas_app.merchant AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.id, 'SET_AS_DEFAULT_EMERGENCY_NUMBER', 'Customer {#whoSetMobileNumber#} set you as his default emergency number.'
  FROM atlas_app.merchant AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.id, 'SEND_OTP', '<#> Your OTP for login to Yatri App is {#otp#} {#hash#}'
  FROM atlas_app.merchant AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message)
  (SELECT * FROM MerchantMessages);