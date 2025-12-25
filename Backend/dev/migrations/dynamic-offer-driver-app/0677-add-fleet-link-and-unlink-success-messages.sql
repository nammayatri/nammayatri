WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'FLEET_LINK_SUCCESS_MESSAGE', 'Operator {#operatorName#} successfully linked to your fleet', T1.id
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'FLEET_UNLINK_SUCCESS_MESSAGE', 'Your fleet has been successfully unlinked from operator {#operatorName#}', T1.id
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);