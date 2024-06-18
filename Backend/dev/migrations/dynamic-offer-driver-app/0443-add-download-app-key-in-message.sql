WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'FLEET_JOIN_AND_DOWNLOAD_APP_MESSAGE' AS message_key,
         'You have been invited to join the fleet of {#fleetOwnerName#}. Download the application to start using it: {# https://nammayatri.in/p/ #}' AS message,
         T1.id AS merchant_operating_city_id
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
  WHERE T1.merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f'
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);