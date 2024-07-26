WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'SEND_RIDE_END_OTP', 'Dear User,

Your ride has started successfully. Please share the end OTP {#otp#} with the driver at the end of the trip to end your ride. -Namma Yatri', T1.id
  FROM atlas_app.merchant_operating_city AS T1
)
INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);