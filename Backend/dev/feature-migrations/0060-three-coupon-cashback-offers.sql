-- Three coupon-code CASHBACK offers for ride-hailing.
--
-- Gives riders three coupons (COUPON code strings) that they can pick from the
-- /offers/list at booking time (carried as `selectedOfferId` through
-- select -> estimate -> quote -> booking). On ride completion the offer's
-- payout amount is credited back to the rider as a RIDE_OFFER_CASHBACK payout
-- (see SharedLogic.Offer.processRideOffer + scheduleCashbackPayoutJob).
--
-- Nothing here is new machinery — it reuses the existing Offer / OfferEntity /
-- cashback-payout pipeline. It is data + config only.
--
-- ┌─────────────────────────────────────────────────────────────────────────┐
-- │ EDIT THESE TWO VALUES for the target merchant + city before applying.     │
-- │ The block no-ops safely (RAISE NOTICE) if the merchant_operating_city     │
-- │ row is not found, so a wrong value cannot corrupt anything.               │
-- └─────────────────────────────────────────────────────────────────────────┘

DO $$
DECLARE
  v_merchant_short_id TEXT := 'NAMMA_MOBILITY';   -- <-- EDIT: merchant short_id
  v_city              TEXT := 'Bangalore';        -- <-- EDIT: city name
  v_merchant_id       TEXT;
  v_city_id           TEXT;
  -- Stable, human-readable offer ids so re-runs are idempotent (ON CONFLICT).
  v_offer_1 TEXT := 'offer-coupon-flat50-cashback';
  v_offer_2 TEXT := 'offer-coupon-flat100-cashback';
  v_offer_3 TEXT := 'offer-coupon-10pct-cashback';
BEGIN
  SELECT moc.merchant_id, moc.id INTO v_merchant_id, v_city_id
  FROM atlas_app.merchant_operating_city moc
  WHERE moc.merchant_short_id = v_merchant_short_id AND moc.city = v_city
  LIMIT 1;

  IF v_merchant_id IS NULL OR v_city_id IS NULL THEN
    RAISE NOTICE '% % not found in merchant_operating_city, skipping coupon setup', v_merchant_short_id, v_city;
    RETURN;
  END IF;

  -- ── Coupon 1: flat ₹50 cashback ──────────────────────────────────────────
  INSERT INTO atlas_app.offer (
    id, offer_code, offer_type, discount_type, discount_value, max_discount,
    title, description, sponsored_by, tnc,
    offer_eligibility_json_logic, currency, is_active,
    merchant_id, merchant_operating_city_id, created_at, updated_at, valid_till
  ) VALUES (
    v_offer_1, 'CASHBACK50', 'CASHBACK', 'FLAT', 50.0, NULL,
    '₹50 Cashback', 'Apply this coupon and get flat ₹50 cashback after your ride.',
    v_merchant_short_id,
    'Cashback credited to your account after ride completion. Cannot be combined with other offers.',
    NULL,                     -- no eligibility restriction (see note at bottom to cap per user)
    'INR', true,
    v_merchant_id, v_city_id, now(), now(), NULL
  ) ON CONFLICT (id) DO UPDATE SET
    offer_code = EXCLUDED.offer_code, discount_value = EXCLUDED.discount_value,
    max_discount = EXCLUDED.max_discount, offer_eligibility_json_logic = EXCLUDED.offer_eligibility_json_logic,
    is_active = EXCLUDED.is_active, updated_at = now();

  -- ── Coupon 2: flat ₹100 cashback ─────────────────────────────────────────
  INSERT INTO atlas_app.offer (
    id, offer_code, offer_type, discount_type, discount_value, max_discount,
    title, description, sponsored_by, tnc,
    offer_eligibility_json_logic, currency, is_active,
    merchant_id, merchant_operating_city_id, created_at, updated_at, valid_till
  ) VALUES (
    v_offer_2, 'CASHBACK100', 'CASHBACK', 'FLAT', 100.0, NULL,
    '₹100 Cashback', 'Apply this coupon and get flat ₹100 cashback after your ride.',
    v_merchant_short_id,
    'Cashback credited to your account after ride completion. Cannot be combined with other offers.',
    NULL,
    'INR', true,
    v_merchant_id, v_city_id, now(), now(), NULL
  ) ON CONFLICT (id) DO UPDATE SET
    offer_code = EXCLUDED.offer_code, discount_value = EXCLUDED.discount_value,
    max_discount = EXCLUDED.max_discount, offer_eligibility_json_logic = EXCLUDED.offer_eligibility_json_logic,
    is_active = EXCLUDED.is_active, updated_at = now();

  -- ── Coupon 3: 10% cashback, capped at ₹75 ────────────────────────────────
  INSERT INTO atlas_app.offer (
    id, offer_code, offer_type, discount_type, discount_value, max_discount,
    title, description, sponsored_by, tnc,
    offer_eligibility_json_logic, currency, is_active,
    merchant_id, merchant_operating_city_id, created_at, updated_at, valid_till
  ) VALUES (
    v_offer_3, 'CASHBACK10PCT', 'CASHBACK', 'PERCENTAGE', 10.0, 75.0,
    '10% Cashback', 'Apply this coupon and get 10% cashback (up to ₹75) after your ride.',
    v_merchant_short_id,
    'Maximum cashback ₹75. Credited after ride completion. Cannot be combined with other offers.',
    NULL,
    'INR', true,
    v_merchant_id, v_city_id, now(), now(), NULL
  ) ON CONFLICT (id) DO UPDATE SET
    offer_code = EXCLUDED.offer_code, discount_value = EXCLUDED.discount_value,
    max_discount = EXCLUDED.max_discount, offer_eligibility_json_logic = EXCLUDED.offer_eligibility_json_logic,
    is_active = EXCLUDED.is_active, updated_at = now();

  RAISE NOTICE 'Three coupon cashback offers created for % %', v_merchant_short_id, v_city;

  -- Cumulative offer policy so all three coupons surface in /offers/list.
  INSERT INTO atlas_app.app_dynamic_logic_element (
    domain, merchant_id, version, logic, description, created_at, updated_at, "order"
  ) VALUES (
    'CUMULATIVE-OFFER-POLICY', v_merchant_id, 1,
    '{"cat":[{"var":""},{"offerIds":["' || v_offer_1 || '","' || v_offer_2 || '","' || v_offer_3 || '"]}]}',
    'Three coupon cashback offers', now(), now(), 0
  ) ON CONFLICT DO NOTHING;

  INSERT INTO atlas_app.app_dynamic_logic_rollout (
    domain, merchant_operating_city_id, percentage_rollout, time_bounds, version,
    version_description, merchant_id, created_at, updated_at
  ) VALUES (
    'CUMULATIVE-OFFER-POLICY', v_city_id, 100, 'Unbounded', 1,
    'Three coupon cashback offers', v_merchant_id, now(), now()
  ) ON CONFLICT DO NOTHING;

  -- Route offer resolution through the in-house (domain) offer engine.
  UPDATE atlas_app.merchant_service_config
  SET config_json = config_json::jsonb || '{"useDomainOffers": true}'::jsonb, updated_at = now()
  WHERE merchant_id = v_merchant_id
    AND merchant_operating_city_id = v_city_id
    AND service_name IN ('Payment_Stripe', 'Payment_StripeTest', 'Payment_Juspay');

  -- Gate that must be on for ride-hailing offers + cash-ride cashback apply.
  UPDATE atlas_app.rider_config
  SET enable_ride_hailing_offers = true, updated_at = now()
  WHERE merchant_operating_city_id = v_city_id;

  RAISE NOTICE 'Cumulative offer policy, useDomainOffers, and enable_ride_hailing_offers set for % %', v_merchant_short_id, v_city;
END $$;

-- ─────────────────────────────────────────────────────────────────────────────
-- OPTIONAL — limit each coupon to one use per rider.
-- The offers above have NO eligibility restriction. To cap a coupon so a rider
-- can redeem it at most once, set offer_eligibility_json_logic to a JsonLogic
-- rule over OfferEligibilityInput. Example (no offer ever used by this rider,
-- same pattern as 0001-helsinki):
--   '{"==":[{"var":"personOfferStats"},[]]}'
-- Per-offer counting (allow using the other two after redeeming one) requires
-- filtering personOfferStats by offerId in JsonLogic; keep NULL unless you need
-- the cap, and validate any rule via the dashboard postOfferValidateEligibility
-- endpoint before rollout.
