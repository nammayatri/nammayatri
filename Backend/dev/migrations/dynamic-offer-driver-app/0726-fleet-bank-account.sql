-- Run for International track in MASTER and PROD
UPDATE atlas_driver_offer_bpp.transporter_config
SET delete_driver_bank_account_when_link_to_fleet = true
WHERE merchant_operating_city_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE city IN ('Amsterdam', 'Helsinki')
);

-- ONLY FOR LOCAL --
INSERT INTO atlas_driver_offer_bpp.driver_bank_account (account_id, charges_enabled, current_account_link, current_account_link_expiry, details_submitted, driver_id, merchant_id, merchant_operating_city_id, created_at, updated_at) VALUES
    ('favorit-auto2-account000000000000000', true, null, null, true, 'favorit-auto2-0000000000000000000000', 'favorit0-0000-0000-0000-00000favorit', 'favorit0-0000-0000-0000-00000000city', now(), now());

-- ONLY FOR LOCAL --
INSERT INTO atlas_driver_offer_bpp.driver_bank_account (account_id, charges_enabled, current_account_link, current_account_link_expiry, details_submitted, driver_id, merchant_id, merchant_operating_city_id, created_at, updated_at) VALUES
    ('favorit-fleet-owner-account000000000', true, null, null, true, 'favorit-fleet-owner-0000000000000000', 'favorit0-0000-0000-0000-00000favorit', 'favorit0-0000-0000-0000-00000000city', now(), now());

-- ONLY FOR LOCAL --
INSERT INTO atlas_driver_offer_bpp.fleet_driver_association (
    id,
    driver_id,
    is_active,
    fleet_owner_id,
    associated_on,
    associated_till,
    created_at,
    updated_at
)
VALUES
    (
        '9a9cf313-8aaf-4546-a4b7-82221a6b47f5',
        'favorit-auto2-0000000000000000000000',
        true,
        'favorit-fleet-owner-0000000000000000',
        now(),
        '2099-12-12',
        now(),
        now()
    );
