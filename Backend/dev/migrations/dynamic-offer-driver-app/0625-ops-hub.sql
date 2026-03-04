-- Local --

INSERT INTO atlas_driver_offer_bpp.operation_hub (
    address, id, lat, lon, merchant_id, merchant_operating_city_id, mobile_number, name, created_at, updated_at
)
SELECT
    'Udyog Vihar Phase 2, Gurugram, Haryana',
    'operation-hub-id-0000000000000000000',
    28.5057,
    77.0888,
    (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'NAMMA_YATRI_PARTNER'),
    (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE city = 'Kochi' AND merchant_short_id = 'NAMMA_YATRI_PARTNER'),
    '7001232129',
    'Udyog Vihar Operation Hub',
    NOW(),
    NOW();

-- Master (run even if commented) --

-- INSERT INTO atlas_driver_offer_bpp.operation_hub (
--     address, id, lat, lon, merchant_id, merchant_operating_city_id, mobile_number, name, created_at, updated_at
-- )
-- SELECT
--     'Udyog Vihar Phase 2, Gurugram, Haryana',
--     atlas_driver_offer_bpp.uuid_generate_v4(),
--     28.5057,
--     77.0888,
--     (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER'),
--     (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE city = 'Delhi' AND merchant_short_id = 'MSIL_PARTNER'),
--     '7001232129',
--     'Udyog Vihar Operation Hub',
--     NOW(),
--     NOW();
