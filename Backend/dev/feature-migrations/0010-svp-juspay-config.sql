-- Seed SVP_Juspay merchant_service_config row for Chennai.
-- Holds the static SVP deduct knobs (programId, burnOptionId, gatewayId,
-- paymentMethod, paymentMethodType) that Tools.LoyaltyWallet.deductSvpFare reads.

DO $$
DECLARE
  v_merchant_id TEXT;
  v_city_id TEXT;
  v_cfg JSONB;
BEGIN
  SELECT moc.merchant_id, moc.id INTO v_merchant_id, v_city_id
  FROM atlas_app.merchant_operating_city moc
  WHERE moc.city = 'Chennai'
  LIMIT 1;

  IF v_merchant_id IS NULL OR v_city_id IS NULL THEN
    RAISE NOTICE 'Chennai merchant_operating_city not found; skipping SVP_Juspay config seed';
    RETURN;
  END IF;

  v_cfg := jsonb_build_object(
    'programId',         '019e06d5-4937-74f2-a8bd-d4bef37584f7',
    'burnOptionId',      '019e06d5-4934-75c1-880d-d0d93c1bc31d',
    'gatewayId',         '1207',
    'paymentMethod',     'LOYALTYOS7',
    'paymentMethodType', 'REWARD'
  );

  IF EXISTS (
    SELECT 1 FROM atlas_app.merchant_service_config
    WHERE merchant_id = v_merchant_id
      AND merchant_operating_city_id = v_city_id
      AND service_name = 'SVP_Juspay'
  ) THEN
    UPDATE atlas_app.merchant_service_config
    SET config_json = v_cfg::json,
        updated_at = now()
    WHERE merchant_id = v_merchant_id
      AND merchant_operating_city_id = v_city_id
      AND service_name = 'SVP_Juspay';
  ELSE
    INSERT INTO atlas_app.merchant_service_config
      (merchant_id, merchant_operating_city_id, service_name, config_json, created_at, updated_at)
    VALUES
      (v_merchant_id, v_city_id, 'SVP_Juspay', v_cfg::json, now(), now());
  END IF;

  RAISE NOTICE 'Chennai SVP_Juspay config seeded';
END $$;
