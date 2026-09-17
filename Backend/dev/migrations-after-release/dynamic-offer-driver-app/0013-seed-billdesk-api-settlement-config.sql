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
          'clientId', '0.1.0|0|dRAXsoGFNRvAXkRjwCwQyAKopjjL1cXnkHdUIllj1j4wDJ1LV2q5clxYj/10HsJYuDhp+Mavissgv15Cw+U4bw==', ------ These are master passetto encrypted. Encrypt again before running in other environments
          'signingKey', '0.1.0|2|nhcUNgF6EMEmUPTBl3pLwEc2u6wc2iNRS4/0TGNWFgCCcVjf+PesHaN7ofuImeVmVoWFDMN1e8L0CRIlcKp/UVz2Osz0G7M7PY9Np5AYKW4UUTt/+rzFOw==',
          'encryptionKey', '0.1.0|2|pYKH1CbfU0Ji5QjDAEjUoVsTHjIRqhe0ws/xima/9EYdG27lRwlDsSKPJKVfOMz/qd8oyKMdWl21dK0+G3DmSLk2jDkT2Ad9iCtruholQGTsuDj/yD4P0g==',
          'encryptionKeyId', '0.1.0|0|dJ6pPpjYqvVmRukhZj+1KexvGF8bk/eBqihSeO7iK2vZJi6wV1cu81r1J+sNRowxp+3SFQJdu15PfmwUBzCAAVaxPp4=',
          'maxDateRangeDays', 7
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
