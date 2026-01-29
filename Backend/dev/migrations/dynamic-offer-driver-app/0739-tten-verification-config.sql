INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'Verification_Tten',
  '{
    "url":"https://tten.wb.gov.in/WBTTANRestAPI"
  }'
FROM atlas_driver_offer_bpp.merchant_operating_city as m
WHERE m.merchant_short_id = 'JATRI_SAATHI_PARTNER'
AND m.city = 'Kolkata';