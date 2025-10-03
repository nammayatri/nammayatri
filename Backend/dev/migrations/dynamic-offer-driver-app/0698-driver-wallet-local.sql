-- ONLY FOR LOCAL
UPDATE atlas_driver_offer_bpp.transporter_config
SET enable_wallet_payout = true
WHERE merchant_operating_city_id='favorit0-0000-0000-0000-00000000city';

-- ONLY FOR LOCAL
UPDATE atlas_driver_offer_bpp.merchant
SET enforce_sufficient_driver_balance = true
WHERE merchant.id='favorit0-0000-0000-0000-00000favorit';

-- ONLY FOR LOCAL
UPDATE atlas_driver_offer_bpp.person
SET merchant_operating_city_id = 'favorit0-0000-0000-0000-00000000city'
WHERE merchant_id = 'favorit0-0000-0000-0000-00000favorit';

-- ONLY FOR LOCAL
UPDATE atlas_driver_offer_bpp.driver_information
SET merchant_operating_city_id = 'favorit0-0000-0000-0000-00000000city'
WHERE driver_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.person
    WHERE merchant_id = 'favorit0-0000-0000-0000-00000favorit'
);
