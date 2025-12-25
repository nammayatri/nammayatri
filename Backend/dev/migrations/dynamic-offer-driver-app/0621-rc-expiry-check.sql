UPDATE atlas_driver_offer_bpp.transporter_config set rc_expiry_checks = true WHERE merchant_operating_city_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
    AND city = 'Delhi'
);


UPDATE atlas_driver_offer_bpp.merchant set fleet_owner_enabled_check = true where short_id = 'MSIL_PARTNER';