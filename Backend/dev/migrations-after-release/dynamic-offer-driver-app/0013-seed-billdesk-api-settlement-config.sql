-- Seed BillDesk API settlement service config
-- Updates existing Settlement_BillDesk config to use BillDeskApiSourceConfig
-- instead of SFTP/Email source. Encrypted fields must be replaced with
-- actual encrypted values before running in production.

UPDATE atlas_driver_offer_bpp.merchant_service_config
SET config_json = jsonb_build_object(
      'settlementService', 'BillDesk',
      'bankCode', config_json->>'bankCode',
      'sourceConfig', jsonb_build_object(
        'tag', 'BillDeskApiSourceConfig',
        'contents', jsonb_build_object(
          'baseUrl', 'https://apimh.billdesk.com',
          'merchantId', 'JPMSILRIDE',
          'clientId', '<ENCRYPTED_CLIENT_ID>',
          'signingKey', '<ENCRYPTED_SIGNING_KEY>',
          'encryptionKey', '<ENCRYPTED_ENCRYPTION_KEY>',
          'encryptionKeyId', '<ENCRYPTED_ENCRYPTION_KEY_ID>'
        )
      ),
      'parserTypeMap', null,
      'useJuspayOrderStatus', null
    ),
    updated_at = NOW()
WHERE service_name = 'Settlement_BillDesk'
  AND merchant_operating_city_id IN (
    SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
  );
