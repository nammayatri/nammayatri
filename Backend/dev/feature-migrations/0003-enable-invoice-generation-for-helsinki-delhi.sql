UPDATE atlas_driver_offer_bpp.merchant SET prepaid_subscription_and_wallet_enabled = true WHERE short_id IN ('BRIDGE_FINLAND_PARTNER', 'BHARAT_TAXI_PARTNER');

UPDATE atlas_driver_offer_bpp.transporter_config
SET driver_wallet_config = jsonb_set(
  driver_wallet_config::jsonb,
  '{enableDriverWallet}', 'true'
)
WHERE merchant_operating_city_id IN (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_short_id IN ('BRIDGE_FINLAND_PARTNER', 'BHARAT_TAXI_PARTNER') AND city IN ('Helsinki', 'Delhi'));
