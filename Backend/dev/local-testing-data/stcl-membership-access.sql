-- STCL Membership flow setup for NAMMA_YATRI_PARTNER integration tests (idempotent).
-- Applied by test-context-api startup (local-testing-data batch).
--
-- The StclMembershipFlow collection's "Submit Application" step creates a
-- payment order via MembershipPayment_Juspay. This config is missing for
-- NAMMA_YATRI_PARTNER (only BHARAT_TAXI_PARTNER and MSIL_PARTNER have it
-- in production). Without it the driver-app returns:
--   E500 MERCHANT_SERVICE_CONFIG_NOT_FOUND: MembershipPayment_Juspay

INSERT INTO atlas_driver_offer_bpp.merchant_service_config
  (service_name, merchant_id, merchant_operating_city_id, config_json)
VALUES (
  'MembershipPayment_Juspay',
  '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f',
  '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98',
  '{"apiKey": "0.1.0|2|5ZL4MNhQ2ynjgp/RjacXY6Sd8Bl2SV0k3TFvu/VHPGvjK/lX16LumiEn4z/iR/k9/jsvCvtjD2ifKI2mrz0=", "autoRefundConflictThresholdMinutes": null, "gatewayReferenceId": null, "isPercentageSplit": null, "isRefundSplitEnabled": null, "isSplitEnabled": null, "merchantId": "nammayatri", "mockStatusUrl": null, "offerSKUConfig": null, "password": "0.1.0|0|8bgi6wN6K9Hx0R6whcRbd1Bar6Zm4zMtLjPdlHP86TRNyZVD7f4HeK15+tay3wxHfEFZPKtqtOOmO8NwN+qz9Q==", "paymentOrderValidity": null, "pseudoClientId": null, "returnUrl": "http://localhost:8080/juspay/end", "serviceMode": null, "url": "http://localhost:8080/juspay", "username": "nammayatri", "walletIssuer": null, "walletRewardApiVersion": null}'
) ON CONFLICT DO NOTHING;
