-- ONLY FOR LOCAL
UPDATE atlas_driver_offer_bpp.transporter_config
SET enable_driver_wallet = true,
    enable_tds_deduction = true
WHERE merchant_operating_city_id='favorit0-0000-0000-0000-00000000city';

-- ONLY FOR LOCAL
UPDATE atlas_driver_offer_bpp.merchant
SET enforce_sufficient_driver_balance = true
WHERE merchant.id='favorit0-0000-0000-0000-00000favorit';

-- ONLY FOR LOCAL
UPDATE atlas_driver_offer_bpp.driver_information
SET prepaid_subscription_balance = 1000,
    plan_expiry_date = '2099-01-01 00:00:00.00000+00'
WHERE driver_id='favorit-auto2-0000000000000000000000';
