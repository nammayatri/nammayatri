-- Append ride-revenue GL keys to SAP_Journal accountMapping (WS3 ERP posting).
-- Placeholder hkont/kostl/prctr — replace with real SAP GL codes before prod enablement.
-- Key names follow existing "… A/C" convention used by SAPReportDispatch mkItem lookups.

UPDATE atlas_driver_offer_bpp.merchant_service_config AS msc
SET
    config_json = jsonb_set(
        msc.config_json,
        '{accountMapping}',
        COALESCE(msc.config_json -> 'accountMapping', '{}'::jsonb) || '{
          "BUYER_APP_RECEIVABLE A/C": {
            "hkont": "1005000",
            "kostl": "CC001",
            "prctr": "PC001"
          },
          "BUYER_APP_POOL A/C": {
            "hkont": "1006000",
            "kostl": "CC001",
            "prctr": "PC001"
          },
          "RIDE_FARE_REVENUE A/C": {
            "hkont": "1007000",
            "kostl": "CC001",
            "prctr": "PC001"
          },
          "DRIVER_BALANCE A/C": {
            "hkont": "1008000",
            "kostl": "CC001",
            "prctr": "PC001"
          },
          "PAYOUT_CLEARING A/C": {
            "hkont": "1009000",
            "kostl": "CC001",
            "prctr": "PC001"
          },
          "TDS_PAYABLE A/C": {
            "hkont": "1010000",
            "kostl": "CC001",
            "prctr": "PC001"
          },
          "TDS_RECEIVABLE A/C": {
            "hkont": "1011000",
            "kostl": "CC001",
            "prctr": "PC001"
          }
        }'::jsonb
    ),
    updated_at = NOW()
FROM atlas_driver_offer_bpp.merchant_operating_city AS moc
WHERE msc.merchant_operating_city_id = moc.id
  AND msc.merchant_id = moc.merchant_id
  AND msc.service_name = 'SAP_Journal'
  AND moc.merchant_short_id = 'MSIL_PARTNER'
  AND moc.city = 'Hyderabad';
