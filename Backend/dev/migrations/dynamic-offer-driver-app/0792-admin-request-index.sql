CREATE INDEX IF NOT EXISTS idx_admin_request_reference_id ON atlas_driver_offer_bpp.admin_request (reference_id);

-- ONLY FOR LOCAL --
UPDATE atlas_driver_offer_bpp.merchant
SET prepaid_subscription_and_wallet_enabled = true
WHERE id = 'favorit0-0000-0000-0000-00000favorit';

-- ONLY FOR LOCAL --
UPDATE atlas_driver_offer_bpp.transporter_config
SET driver_wallet_config = (driver_wallet_config::jsonb || '{"enableDriverWallet": true,"forceOnlineLedger": true}'::jsonb)::json
WHERE merchant_operating_city_id = 'favorit0-0000-0000-0000-00000000city'
