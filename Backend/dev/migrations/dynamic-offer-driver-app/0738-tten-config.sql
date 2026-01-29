INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'Tokenization_Tten',
  '{
    "url":"https://tten.wb.gov.in/WBTTANRestAPI",
    "username":"superadmin",
    "password":"0.1.0|0|L4bwTzrInb7XoT/2UEBUS1rWXFFwktWUt8uRZ0VYQjpamZ3tqdMV8iZYo/6F3/3X9LQwknkrlRj9"
  }'   --This password is master passetto encrypted. Please re-encrypt before running in prod.
FROM atlas_driver_offer_bpp.merchant_operating_city as m
WHERE m.merchant_short_id = 'JATRI_SAATHI_PARTNER'
AND m.city = 'Kolkata';