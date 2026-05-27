UPDATE atlas_driver_offer_bpp.transporter_config
SET enable_admin_maker_checker = ARRAY['LedgerAdjustment', 'FailedPayoutReTrigger', 'TDSReimbursement']::text[]
WHERE merchant_operating_city_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
    AND city = 'Delhi'
);

-- ONLY FOR LOCAL --
UPDATE atlas_driver_offer_bpp.transporter_config
SET enable_admin_maker_checker = ARRAY['LedgerAdjustment', 'FailedPayoutReTrigger', 'TDSReimbursement']::text[]
WHERE merchant_operating_city_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'NAMMA_YATRI_PARTNER'
    AND city = 'Bangalore'
);

-- ONLY FOR LOCAL --
UPDATE atlas_driver_offer_bpp.merchant
SET prepaid_subscription_and_wallet_enabled = true
WHERE short_id = 'NAMMA_YATRI_PARTNER';
