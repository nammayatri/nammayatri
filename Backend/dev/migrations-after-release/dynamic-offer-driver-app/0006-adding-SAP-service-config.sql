-- SAP Journal Service Config
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (
    merchant_id,
    merchant_operating_city_id,
    service_name,
    config_json,
    created_at,
    updated_at
)
SELECT
    merchant_id,
    id,
    'SAP_Journal',
    '{
      "sapAuthUrl": "https://oauthasservices-l6ef3ac9c.ae1.hana.ondemand.com",
      "sapApiUrl": "https://e550116-iflmap.hcisbt.ae1.hana.ondemand.com",
      "sapAuthCredentials": "0.1.0|2|PLACEHOLDER_ENCRYPTED_CREDENTIALS",
      "bukrs": "2000",
      "blart": "zw",
      "accountMapping": {
        "PG_CLEARING A/C": {
          "hkont": "1001000",
          "kostl": "CC001",
          "prctr": "PC001"
        },
        "DEFERRED_REVENUE A/C": {
          "hkont": "1002000",
          "kostl": "CC001",
          "prctr": "PC001"
        },
        "CGST_PAYABLE A/C": {
          "hkont": "1003000",
          "kostl": "CC001",
          "prctr": "PC001"
        },
        "SGST_PAYABLE A/C": {
          "hkont": "1003000",
          "kostl": "CC001",
          "prctr": "PC001"
        },
        "IGST_PAYABLE A/C": {
          "hkont": "1003000",
          "kostl": "CC001",
          "prctr": "PC001"
        },
        "BANK A/C": {
          "hkont": "1004000",
          "kostl": "CC001",
          "prctr": "PC001"
        }
      }
    }'::jsonb,
    NOW(),
    NOW()
FROM atlas_driver_offer_bpp.merchant_operating_city
WHERE merchant_short_id = 'MSIL_PARTNER';
