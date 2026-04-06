-- Delhi (BHARAT_TAXI) — 10% Cashback offer for first ride (Offline/Cash Payment)
-- Eligible only for customers with 0 previous offer applications

DO $$
DECLARE
  v_merchant_id TEXT;
  v_city_id TEXT;
  v_offer_id TEXT := 'offer-del-cash-10pct-first-ride';
BEGIN
  SELECT m.id, moc.id INTO v_merchant_id, v_city_id
  FROM atlas_app.merchant m
  JOIN atlas_app.merchant_operating_city moc ON moc.merchant_id = m.id
  WHERE m.short_id = 'BHARAT_TAXI' AND moc.city = 'Delhi'
  LIMIT 1;

  IF v_merchant_id IS NULL OR v_city_id IS NULL THEN
    RAISE NOTICE 'BHARAT_TAXI Delhi not found in merchant_operating_city, skipping offer setup';
    RETURN;
  END IF;

  -- Insert 10% cashback offer (first ride only)
  INSERT INTO atlas_app.offer (
    id, offer_code, offer_type, discount_type, discount_value, max_discount,
    title, description, sponsored_by, tnc,
    offer_eligibility_json_logic,
    currency, is_active,
    merchant_id, merchant_operating_city_id,
    created_at, updated_at
  ) VALUES (
    v_offer_id,
    'FIRST_RIDE_10PCT_CASHBACK',
    'CASHBACK',
    'PERCENTAGE',
    10.0,          -- 10% cashback
    5000.0,        -- max cashback capped at 5000 paise (₹50)
    'First Ride Cashback',
    'Get 10% cashback on your first ride! Maximum cashback of ₹50.',
    'BHARAT_TAXI',
    'Valid for first ride only. Cashback credited after ride completion. Cannot be combined with other offers.',
    NULL,  -- no eligibility restriction for dev testing
    'INR',
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

  RAISE NOTICE 'Delhi offline payment 10%% cashback offer created: %', v_offer_id;

  -- Create cumulative offer policy dynamic logic for Delhi
  INSERT INTO atlas_app.app_dynamic_logic_element (
    domain, merchant_id, version, logic, description, created_at, updated_at, "order"
  ) VALUES (
    'CUMULATIVE-OFFER-POLICY',
    v_merchant_id,
    1,
    '{"cat":[{"var":""},{"offerTitle":"First Ride Cashback"},{"offerDescription":"Get 10% cashback on your first ride! Maximum cashback of ₹50."},{"offerSponsoredBy":["BHARAT_TAXI"]},{"offerIds":["' || v_offer_id || '"]}]}',
    'First Ride Cashback - Delhi',
    now(),
    now(),
    0
  ) ON CONFLICT DO NOTHING;

  -- Rollout the logic at 100%
  INSERT INTO atlas_app.app_dynamic_logic_rollout (
    domain, merchant_operating_city_id, percentage_rollout, time_bounds, version, version_description, merchant_id, created_at, updated_at
  ) VALUES (
    'CUMULATIVE-OFFER-POLICY',
    v_city_id,
    100,
    'Unbounded',
    1,
    'First Ride Cashback',
    v_merchant_id,
    now(),
    now()
  ) ON CONFLICT DO NOTHING;

  RAISE NOTICE 'Delhi cumulative offer policy logic and rollout created';
END $$;
