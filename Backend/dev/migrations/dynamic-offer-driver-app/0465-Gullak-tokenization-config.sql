INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'Tokenization_Gullak',
 json_build_object(
   'url' , 'https://dev.api.gullak.money'
   ,'merchantId' , 'nammayatri'
   ,'apiKey' , 'dummy-key'
 )
FROM atlas_driver_offer_bpp.merchant_operating_city m;