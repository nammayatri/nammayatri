-- Enable cancellation fee for Helsinki operating city so that user-cancellation
-- after the no-show window triggers the USER_CANCELLATION_DUES dynamic logic
-- and a RideCancellation finance_invoice is generated on the driver ledger.

DO $$
DECLARE
  v_city_id TEXT;
BEGIN
  SELECT moc.id INTO v_city_id
  FROM atlas_driver_offer_bpp.merchant_operating_city moc
  WHERE moc.merchant_short_id = 'BRIDGE_FINLAND_PARTNER' AND moc.city = 'Helsinki'
  LIMIT 1;

  IF v_city_id IS NULL THEN
    RAISE NOTICE 'BRIDGE_FINLAND_PARTNER Helsinki not found, skipping cancellation setup';
    RETURN;
  END IF;

  UPDATE atlas_driver_offer_bpp.transporter_config
  SET can_add_cancellation_fee = true
  WHERE merchant_operating_city_id = v_city_id;

  -- Create one cancellation fare policy per distinct fare_policy_id referenced by
  -- Helsinki fare products, then link each fare_policy to its cancellation policy.
  WITH helsinki_fare_policies AS (
    SELECT DISTINCT fp.id AS fare_policy_id, gen_random_uuid()::text AS cancel_policy_id
    FROM atlas_driver_offer_bpp.fare_product fpr
    JOIN atlas_driver_offer_bpp.fare_policy fp ON fp.id = fpr.fare_policy_id
    WHERE fpr.merchant_operating_city_id = v_city_id
      AND fp.cancellation_fare_policy_id IS NULL
  ),
  inserted_policies AS (
    INSERT INTO atlas_driver_offer_bpp.cancellation_fare_policy (
      id, currency, description,
      free_cancellation_time_seconds,
      max_waiting_time_at_pickup_seconds,
      min_cancellation_charge, max_cancellation_charge,
      per_metre_cancellation_charge, per_minute_cancellation_charge,
      percentage_of_ride_fare_to_be_charged
    )
    SELECT
      cancel_policy_id,
      'EUR',
      'Helsinki cancellation fare policy',
      120,   -- 2 min free cancellation window
      300,   -- 5 min max wait at pickup (no-show)
      5,     -- min €5
      50,    -- max €50
      0.5,   -- €0.50/metre
      0.5,   -- €0.50/min
      '0.1'  -- 10% of ride fare
    FROM helsinki_fare_policies
    RETURNING id, id AS cancel_policy_id
  )
  UPDATE atlas_driver_offer_bpp.fare_policy fp
  SET cancellation_fare_policy_id = hfp.cancel_policy_id
  FROM helsinki_fare_policies hfp
  WHERE fp.id = hfp.fare_policy_id
    AND fp.cancellation_fare_policy_id IS NULL;

  RAISE NOTICE 'Helsinki cancellation fare policy setup complete for city %', v_city_id;
END $$;
