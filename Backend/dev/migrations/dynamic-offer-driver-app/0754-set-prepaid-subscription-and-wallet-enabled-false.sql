-- Set prepaid_subscription_and_wallet_enabled to false for all merchants
UPDATE atlas_driver_offer_bpp.merchant
SET prepaid_subscription_and_wallet_enabled = false;

-- Set prepaid_subscription_and_wallet_enabled to true for MSIL_PARTNER
UPDATE atlas_driver_offer_bpp.merchant
SET prepaid_subscription_and_wallet_enabled = true
WHERE short_id = 'MSIL_PARTNER';
