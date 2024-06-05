WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'FLEET_JOINING_MESSAGE', 'You have been invited to join the fleet of {#fleetOwnerName#}. {#otp#} is your OTP to be part of it.', T1.id
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);