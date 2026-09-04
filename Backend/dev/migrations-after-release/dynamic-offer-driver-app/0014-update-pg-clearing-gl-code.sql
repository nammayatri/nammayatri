UPDATE atlas_driver_offer_bpp.merchant_service_config AS msc
SET
    config_json = jsonb_set(
        msc.config_json::jsonb,
        '{accountMapping,PG_CLEARING A/C,hkont}',
        '"2421410"'::jsonb
    )::json,
    updated_at = NOW()
FROM atlas_driver_offer_bpp.merchant_operating_city AS moc
WHERE msc.merchant_operating_city_id = moc.id
  AND msc.merchant_id = moc.merchant_id
  AND msc.service_name = 'SAP_Journal'
  AND moc.merchant_short_id = 'MSIL_PARTNER'
  AND moc.city = 'Hyderabad';
