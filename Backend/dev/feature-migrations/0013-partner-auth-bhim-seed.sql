-- Seed PartnerAuth_BHIM merchant_service_config row for the NAMMA_YATRI merchant
-- (default operating city) so the BHIM PWA-journey facade can resolve its
-- provider config via MerchantServiceConfig.
--
-- NOTE: `aesKey` is an EncryptedField — the { "encrypted", "hash" } values below
-- are PLACEHOLDERS. Before any real use, replace them with the BHIM AES key
-- encrypted via the configured encryption service (passetto), and set
-- baseUrl / partnerId to the real per-environment values (UAT / prod).
-- The build does not depend on these values.

DO $$
DECLARE
  v_merchant_id TEXT;
  v_city_id TEXT;
BEGIN
  SELECT moc.merchant_id, moc.id INTO v_merchant_id, v_city_id
  FROM atlas_app.merchant_operating_city moc
  JOIN atlas_app.merchant m ON m.id = moc.merchant_id
  WHERE m.short_id = 'NAMMA_YATRI'
    AND moc.city = m.default_city -- runtime resolves config by the merchant's default city
  LIMIT 1;

  IF v_merchant_id IS NULL OR v_city_id IS NULL THEN
    RAISE NOTICE 'NAMMA_YATRI merchant_operating_city not found; skipping PartnerAuth_BHIM config seed';
    RETURN;
  END IF;

  INSERT INTO atlas_app.merchant_service_config
    (merchant_id, merchant_operating_city_id, service_name, config_json, created_at, updated_at)
  VALUES
    ( v_merchant_id,
      v_city_id,
      'PartnerAuth_BHIM',
      jsonb_build_object(
        'baseUrl',   'https://uat.bhim.example.com',           -- PLACEHOLDER (per-env BHIM base URL)
        'partnerId', 'REPLACE_WITH_BHIM_PARTNER_ID',           -- PLACEHOLDER (per-env)
        'aesKey',    jsonb_build_object(                        -- EncryptedField 'AsEncrypted Text
          'encrypted', 'REPLACE_WITH_ENCRYPTED_BHIM_AES_KEY',  -- PLACEHOLDER (passetto-encrypted)
          'hash',      'REPLACE_WITH_AES_KEY_HASH'              -- PLACEHOLDER
        )
      )::json,
      now(),
      now()
    )
  ON CONFLICT (service_name, merchant_operating_city_id) DO UPDATE SET -- matches the city-aware PK
    config_json = EXCLUDED.config_json,
    updated_at = now();

  RAISE NOTICE 'Seeded PartnerAuth_BHIM merchant_service_config for NAMMA_YATRI (merchant %, city %)', v_merchant_id, v_city_id;
END $$;
