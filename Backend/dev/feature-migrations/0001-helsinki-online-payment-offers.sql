-- Helsinki (BRIDGE_FINLAND) — 10% Discount offer for first ride (Online Payment)
-- Eligible only for customers with 0 previous offer applications

DO $$
DECLARE
  v_merchant_id TEXT;
  v_city_id TEXT;
  v_offer_id TEXT := 'offer-hel-disc-10pct-first-ride';
BEGIN
  SELECT moc.merchant_id, moc.id INTO v_merchant_id, v_city_id
  FROM atlas_app.merchant_operating_city moc
  WHERE moc.merchant_short_id = 'BRIDGE_FINLAND' AND moc.city = 'Helsinki'
  LIMIT 1;

  IF v_merchant_id IS NULL OR v_city_id IS NULL THEN
    RAISE NOTICE 'BRIDGE_FINLAND Helsinki not found in merchant_operating_city, skipping offer setup';
    RETURN;
  END IF;

  -- Insert 10% discount offer (first ride only)
  INSERT INTO atlas_app.offer (
    id, offer_code, offer_type, discount_type, discount_value, max_discount,
    title, description, sponsored_by, tnc,
    offer_eligibility_json_logic,
    currency, is_active,
    merchant_id, merchant_operating_city_id,
    created_at, updated_at
  ) VALUES (
    v_offer_id,
    'FIRST_RIDE_100PCT_DISCOUNT',
    'DISCOUNT',
    'PERCENTAGE',
    100.0,         -- 100% discount
    10.0,          -- max discount capped at €10
    'First Ride Free',
    'Your first ride is on us! 100% off up to €10.',
    'BRIDGE_FINLAND',
    'Valid for first ride only. Maximum discount €10. Cannot be combined with other offers.',
    '{"and":[{">=":[{"var":"personStats.completedRides"},0]},{"==":[{"var":"personOfferStats"},[]]}]}',  -- eligible from first ride AND no previous offer usage
    'EUR',
    true,
    v_merchant_id,
    v_city_id,
    now(),
    now()
  ) ON CONFLICT (id) DO UPDATE SET
    discount_value = EXCLUDED.discount_value,
    max_discount = EXCLUDED.max_discount,
    offer_eligibility_json_logic = EXCLUDED.offer_eligibility_json_logic,
    is_active = EXCLUDED.is_active,
    updated_at = now();

  RAISE NOTICE 'Helsinki online payment 10%% discount offer created: %', v_offer_id;

  -- Enable useDomainOffers in Payment_Stripe, Payment_StripeTest, Payment_Juspay for Helsinki
  UPDATE atlas_app.merchant_service_config
  SET config_json = config_json::jsonb || '{"useDomainOffers": true}'::jsonb,
      updated_at = now()
  WHERE merchant_id = v_merchant_id
    AND merchant_operating_city_id = v_city_id
    AND service_name IN ('Payment_Stripe', 'Payment_StripeTest', 'Payment_Juspay');

  RAISE NOTICE 'Helsinki: useDomainOffers set to true for Stripe/StripeTest/Juspay';
END $$;
