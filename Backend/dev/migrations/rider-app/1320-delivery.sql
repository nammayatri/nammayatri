WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'SMS_DELIVERY_DETAILS_SENDER', 'You parcel delivery start otp is {#otp#} and you can track the delivery on this url {#trackingUrl#}. Your delivery partner is {#driverName#}. To contact him call {#driverNumber#}', T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);


WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'SMS_DELIVERY_DETAILS_RECEIVER', 'You parcel delivery end otp is {#otp#} and you can track the delivery on this url {#trackingUrl#}. Your delivery partner is {#driverName#}. To contact him call {#driverNumber#}. Please check the delivery item before giving end otp.', T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);
