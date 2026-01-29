UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET toto_verification_priority_list = '{"Tten"}'
WHERE merchant_operating_city_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'JATRI_SAATHI_PARTNER'
    AND city = 'Kolkata'
);
