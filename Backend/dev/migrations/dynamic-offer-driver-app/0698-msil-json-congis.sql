UPDATE atlas_driver_offer_bpp.transporter_config
SET subscription_config = '{"prepaidSubscriptionThreshold": 100}' :: json
WHERE merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id = 'MSIL_PARTNER' and city = 'Delhi');

UPDATE atlas_driver_offer_bpp.transporter_config
SET driver_wallet_config = '{"enableDriverWallet": true, "driverWalletPayoutThreshold": 0, "gstPercentage": 5.0, "enableWalletPayout": true, "enableWalletTopup": true, "maxWalletPayoutsPerDay": null, "minWalletAmountForCashRides": null, "minimumWalletPayoutAmount": 0}' :: json
WHERE merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id = 'MSIL_PARTNER' and city = 'Delhi');

UPDATE atlas_driver_offer_bpp.transporter_config
SET analytics_config = '{"weekStartMode": 3, "earningsWindowSize": 7, "allowCacheDriverFlowStatus": true, "enableFleetOperatorDashboardAnalytics": true, "maxOnlineDurationDays": 10, "onlineDurationCalculateFrom": "2025-01-01 00:00:00.000000+00"}' :: json
WHERE merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id = 'MSIL_PARTNER' and city = 'Delhi');
