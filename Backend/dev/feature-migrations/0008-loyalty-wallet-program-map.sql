-- Seed JuspayWallet_Juspay merchant_service_config row for Chennai with
-- a loyaltyProgramMap entry mapping the test programId to LOYALTY_WALLET.
-- Required for the LoyaltyWalletFlow integration tests:
--   loadLoyaltyProgramMap looks up service_name = 'JuspayWallet_Juspay' and
--   reads jcfg.loyaltyProgramMap to resolve programId -> CounterpartyType.

DO $$
DECLARE
  v_merchant_id TEXT;
  v_city_id TEXT;
  v_program_id TEXT := '019d9617-abeb-7a92-ac58-0d58052508c4';
  v_existing_cfg JSONB;
BEGIN
  SELECT moc.merchant_id, moc.id INTO v_merchant_id, v_city_id
  FROM atlas_app.merchant_operating_city moc
  WHERE moc.city = 'Chennai'
  LIMIT 1;

  IF v_merchant_id IS NULL OR v_city_id IS NULL THEN
    RAISE NOTICE 'Chennai merchant_operating_city not found; skipping loyalty wallet config seed';
    RETURN;
  END IF;

  -- Borrow the JuspayCfg shape from the existing Payment_Juspay row, then add loyaltyProgramMap.
  SELECT config_json::jsonb INTO v_existing_cfg
  FROM atlas_app.merchant_service_config
  WHERE merchant_id = v_merchant_id
    AND service_name = 'Payment_Juspay'
  LIMIT 1;

  IF v_existing_cfg IS NULL THEN
    -- Fallback minimal JuspayCfg if no Payment_Juspay row exists yet.
    v_existing_cfg := jsonb_build_object(
      'apiKey',           'mock-juspay-api-key',
      'username',         'mock',
      'password',         'mock',
      'merchantId',       'CUMTA',
      'url',              'http://localhost:8080/juspay/',
      'returnUrl',        'http://localhost:8080/juspay/return',
      'walletRewardApiVersion', '2'
    );
  END IF;

  -- Merge loyaltyProgramMap into the JuspayCfg.
  -- Also FORCE url / walletRewardApiVersion to point at the local mock server,
  -- otherwise loyaltyInfo / order-status calls hit real Juspay (PASETO auth fails).
  v_existing_cfg := v_existing_cfg
    || jsonb_build_object(
         'loyaltyProgramMap',      jsonb_build_object(v_program_id, 'LOYALTY_WALLET'),
         'url',                    'http://localhost:8080/juspay/',
         'returnUrl',              'http://localhost:8080/juspay/return',
         'walletRewardApiVersion', '2'
       );

  IF EXISTS (
    SELECT 1 FROM atlas_app.merchant_service_config
    WHERE merchant_id = v_merchant_id
      AND merchant_operating_city_id = v_city_id
      AND service_name = 'JuspayWallet_Juspay'
  ) THEN
    UPDATE atlas_app.merchant_service_config
    SET config_json = (
          config_json::jsonb
          || jsonb_build_object(
               'loyaltyProgramMap',      jsonb_build_object(v_program_id, 'LOYALTY_WALLET'),
               'url',                    'http://localhost:8080/juspay/',
               'returnUrl',              'http://localhost:8080/juspay/return',
               'walletRewardApiVersion', '2'
             )
        )::json,
        updated_at = now()
    WHERE merchant_id = v_merchant_id
      AND merchant_operating_city_id = v_city_id
      AND service_name = 'JuspayWallet_Juspay';
  ELSE
    INSERT INTO atlas_app.merchant_service_config
      (merchant_id, merchant_operating_city_id, service_name, config_json, created_at, updated_at)
    VALUES
      (v_merchant_id, v_city_id, 'JuspayWallet_Juspay', v_existing_cfg::json, now(), now());
  END IF;

  RAISE NOTICE 'Chennai JuspayWallet_Juspay config seeded (programId % -> LOYALTY_WALLET)', v_program_id;
END $$;
