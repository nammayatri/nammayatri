-- date in past means that calculation already switched on (nothing will change in logic)
UPDATE atlas_driver_offer_bpp.transporter_config
SET online_duration_calculate_from = '2025-01-01 00:00:00.000000+00'
WHERE merchant_operating_city_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
    AND city = 'Delhi'
);
dslsamdlas