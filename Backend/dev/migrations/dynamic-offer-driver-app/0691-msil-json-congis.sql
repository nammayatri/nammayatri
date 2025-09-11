UPDATE atlas_driver_offer_bpp.transporter_config
SET analytics_config = '{"allowCacheDriverFlowStatus": true}' :: json
WHERE merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id = 'MSIL_PARTNER' and city = 'Delhi');
