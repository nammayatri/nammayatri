-- Seed the Aarokya PartnerSdk service config for rider-app.
--
-- Required so rider-app's POST /aarokya/contributorToken can resolve the Aarokya
-- backend URL + platform Basic-auth credential. Tools.PartnerSdk.generateContributorToken
-- looks up service_name = 'PartnerSdk_Aarokya' for (merchantId, merchantOperatingCityId).
--
-- No DDL needed — merchant_service_config already exists and service_name is text.
--
-- Config shape: this decodes into
--   AarokyaSdkConfig { url :: BaseUrl, basicToken :: EncryptedField 'AsEncrypted Text }
-- so the encrypted credential MUST be under the key "basicToken". (The older
-- driver-app migration used "apiKey" + "platformId": that was the pre-#1244
-- shared-kernel, which sent an `api-key:` header. shared-kernel PR #1244
-- "aarokya-headerchanges" replaced it with `Authorization: Basic <basicToken>`,
-- which is what the current code sends.) "platformId" is kept below for
-- traceability only — it is ignored by the decoder, since the platform identity
-- is already inside the Basic credential's client_id.
--
-- Same url + credential as the driver app (same Aarokya backend / platform account).
--
-- NOTE: the primary key is (merchant_id, service_name) — it does NOT include
-- merchant_operating_city_id, so only ONE row per merchant is possible. The
-- lookup filters on the operating city too, so the city selected here is the
-- only one that will resolve this config. Scoped to Kochi to match the
-- driver-app Aarokya pilot; adjust the WHERE clause to roll out elsewhere.

INSERT INTO atlas_app.merchant_service_config
  (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'PartnerSdk_Aarokya',
  json_build_object(
      'url', 'https://aarokya.sandbox.juspay.in/'
    , 'basicToken', '0.1.0|2|S5jEpEGqTAXxcL1ZEFp1GoEaBglJ/cUiLwhnqFLy1xAjFY7n1CSdxRfk0G7QDX5rbL/jcn1g3fAzFtIFegI='
    , 'platformId', '019db4e2-5458-7e42-a1fb-91eec5d83bac'
  )
FROM atlas_app.merchant_operating_city m
WHERE m.merchant_short_id = 'NAMMA_YATRI' AND m.city = 'Kochi'
ON CONFLICT DO NOTHING;
