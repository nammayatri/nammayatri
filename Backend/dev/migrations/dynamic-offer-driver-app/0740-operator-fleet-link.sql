UPDATE atlas_driver_offer_bpp.transporter_config SET allow_multi_fleet_operator_link = true

UPDATE atlas_driver_offer_bpp.transporter_config SET allow_multi_fleet_operator_link = false WHERE merchant_operating_city_id IN (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_short_id = 'MSIL_PARTNER' AND city = 'Delhi');