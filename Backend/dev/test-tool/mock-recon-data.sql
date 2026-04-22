-- mock-recon-data.sql — insert mock rows for local reconciliation readiness + jobs
-- Target schema: atlas_driver_offer_bpp (driver BPP / atlas_dev).
--
-- Prerequisites:
--   - At least one booking row (any status) with merchant_operating_city_id set (clone source).
--   - At least one finance_account row for that city (two preferred for distinct from/to).
--   - A driver for ride rows: either template booking has a ride (driver_id taken from it), or a
--     DRIVER person exists for that city (or any city as fallback).
--   - For subscription / PG payment / payout sections: same DRIVER + a plan for merchant_op_city_id.
--
-- Usage (psql):
--   psql "postgresql://USER@HOST:PORT/atlas_dev" -v ON_ERROR_STOP=1 -f Backend/dev/test-tool/mock-recon-data.sql
--
-- Edit recon_day inside the DO block below, then run the whole file (BEGIN … COMMIT).
--
-- Readiness checks: Backend/dev/test-tool/context-api/server.py build_readiness()
-- PG payment row shape: seed_payment_settlement() in the same file.

BEGIN;

DO $$
DECLARE
  -- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  recon_day date := CURRENT_DATE;  -- change to match dashboard recon date (UTC calendar day)
  -- <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  day_start timestamptz := (recon_day::text || ' 00:00:00+00')::timestamptz;
  day_end timestamptz := (recon_day::text || ' 23:59:59+00')::timestamptz;
  day_mid timestamptz := day_start + interval '12 hours';

  tpl                     atlas_driver_offer_bpp.booking%ROWTYPE;
  ride_tpl                atlas_driver_offer_bpp.ride%ROWTYPE;
  ride_tpl_found          boolean := false;
  acc_from_id             text;
  acc_to_id               text;
  bid_completed           text := gen_random_uuid()::text;
  bid_cancelled           text := gen_random_uuid()::text;
  rid_completed           text := gen_random_uuid()::text;
  rid_cancelled           text := gen_random_uuid()::text;
  loc_from_id             text := gen_random_uuid()::text;
  loc_to_id               text := gen_random_uuid()::text;
  lat_from                double precision := 9.9312;
  lon_from                double precision := 76.2673;
  lat_to                  double precision := 9.9400;
  lon_to                  double precision := 76.2700;
  ledger_booking_id       text := gen_random_uuid()::text;
  merchant_id_val         text;

  driver_id_val           text;
  ride_driver_id          text;
  moc_id_val              text;
  plan_rec                atlas_driver_offer_bpp.plan%ROWTYPE;
  sub_id                  text := gen_random_uuid()::text;
  pay_order_id_val        text := gen_random_uuid()::text;
  ledger_sub_id           text := gen_random_uuid()::text;
  pgpay_id                text := 'recon-pg-' || substr(replace(gen_random_uuid()::text, '-', ''), 1, 12);
  settlement_id_val       text := 'SETTLE-' || recon_day::text || '-MOCK';
  settlement_ts           timestamptz := day_mid + interval '20 hours';
  plan_fee_val            double precision;

  payout_id               text := gen_random_uuid()::text;
  ppsr_id                 text := gen_random_uuid()::text;
  payout_order_id         text := 'payout-ord-' || substr(replace(payout_id, '-', ''), 1, 12);
  plan_ok                 boolean := false;
  sub_merchant_id         text;
BEGIN
  -- Template booking (clone FK targets: fare_parameters, quote, etc.)
  SELECT * INTO tpl
  FROM atlas_driver_offer_bpp.booking b
  WHERE b.merchant_operating_city_id IS NOT NULL
  LIMIT 1;

  IF NOT FOUND THEN
    RAISE EXCEPTION 'mock-recon-data: need at least one booking with merchant_operating_city_id';
  END IF;

  merchant_id_val := tpl.provider_id;
  moc_id_val := tpl.merchant_operating_city_id;

  SELECT fa.id INTO acc_from_id
  FROM atlas_driver_offer_bpp.finance_account fa
  WHERE fa.merchant_operating_city_id = moc_id_val
  ORDER BY fa.id
  LIMIT 1;

  IF acc_from_id IS NULL THEN
    RAISE EXCEPTION 'mock-recon-data: need at least one finance_account for merchant_operating_city_id=%', moc_id_val;
  END IF;

  SELECT fa.id INTO acc_to_id
  FROM atlas_driver_offer_bpp.finance_account fa
  WHERE fa.merchant_operating_city_id = moc_id_val AND fa.id <> acc_from_id
  ORDER BY fa.id
  LIMIT 1;

  acc_to_id := COALESCE(acc_to_id, acc_from_id);

  -- Optional template ride (same booking id as tpl) — reuse driver / fare / trip fields
  SELECT * INTO ride_tpl
  FROM atlas_driver_offer_bpp.ride r
  WHERE trim(r.booking_id::text) = trim(tpl.id::text)
  LIMIT 1;
  ride_tpl_found := FOUND;

  -- Driver for rides + subscription: prefer ride on template booking, else person
  IF ride_tpl_found THEN
    ride_driver_id := NULLIF(trim(ride_tpl.driver_id::text), '');
  ELSE
    ride_driver_id := NULL;
  END IF;

  IF ride_driver_id IS NULL THEN
    SELECT p.id INTO driver_id_val
    FROM atlas_driver_offer_bpp.person p
    WHERE p.role = 'DRIVER'
      AND p.merchant_operating_city_id = moc_id_val
    LIMIT 1;
    IF driver_id_val IS NULL THEN
      SELECT p.id INTO driver_id_val
      FROM atlas_driver_offer_bpp.person p
      WHERE p.role = 'DRIVER' AND p.merchant_operating_city_id IS NOT NULL
      LIMIT 1;
    END IF;
    ride_driver_id := driver_id_val;
  ELSE
    driver_id_val := ride_driver_id;
  END IF;

  IF ride_driver_id IS NULL THEN
    RAISE EXCEPTION 'mock-recon-data: need a DRIVER person or a ride on the template booking (for ride rows)';
  END IF;

  -- Pickup / drop locations (reuse coordinates from template locations when present)
  IF tpl.from_location_id IS NOT NULL THEN
    SELECT l.lat, l.lon INTO lat_from, lon_from
    FROM atlas_driver_offer_bpp.location l
    WHERE l.id = tpl.from_location_id;
  END IF;
  IF tpl.to_location_id IS NOT NULL THEN
    SELECT l.lat, l.lon INTO lat_to, lon_to
    FROM atlas_driver_offer_bpp.location l
    WHERE l.id = tpl.to_location_id;
  END IF;

  INSERT INTO atlas_driver_offer_bpp.location (
    id, lat, lon, city, country, merchant_id, merchant_operating_city_id, full_address, created_at, updated_at
  ) VALUES (
    loc_from_id, lat_from, lon_from, 'Kochi', 'India', merchant_id_val, moc_id_val, 'recon mock pickup', now(), now()
  );

  INSERT INTO atlas_driver_offer_bpp.location (
    id, lat, lon, city, country, merchant_id, merchant_operating_city_id, full_address, created_at, updated_at
  ) VALUES (
    loc_to_id, lat_to, lon_to, 'Kochi', 'India', merchant_id_val, moc_id_val, 'recon mock drop', now(), now()
  );

  -- DSR_VS_LEDGER: completed + cancelled bookings + ledger in window
  INSERT INTO atlas_driver_offer_bpp.booking (
    id, bap_id, bap_uri, estimated_fare, fare_parameters_id, provider_id, quote_id,
    start_time, status, transaction_id, vehicle_variant, merchant_operating_city_id,
    primary_exophone, from_location_id, to_location_id, created_at, updated_at
  ) VALUES (
    bid_completed, tpl.bap_id, tpl.bap_uri, tpl.estimated_fare, tpl.fare_parameters_id,
    tpl.provider_id, tpl.quote_id, day_mid, 'COMPLETED', gen_random_uuid()::text,
    tpl.vehicle_variant, tpl.merchant_operating_city_id, tpl.primary_exophone,
    loc_from_id, loc_to_id, now(), day_mid
  );

  INSERT INTO atlas_driver_offer_bpp.booking (
    id, bap_id, bap_uri, estimated_fare, fare_parameters_id, provider_id, quote_id,
    start_time, status, transaction_id, vehicle_variant, merchant_operating_city_id,
    primary_exophone, from_location_id, to_location_id, created_at, updated_at
  ) VALUES (
    bid_cancelled, tpl.bap_id, tpl.bap_uri, tpl.estimated_fare, tpl.fare_parameters_id,
    tpl.provider_id, tpl.quote_id, day_mid, 'CANCELLED', gen_random_uuid()::text,
    tpl.vehicle_variant, tpl.merchant_operating_city_id, tpl.primary_exophone,
    loc_from_id, loc_to_id, now(), day_mid
  );

  -- Rides linked to the new bookings (COMPLETED + CANCELLED)
  IF ride_tpl_found THEN
    INSERT INTO atlas_driver_offer_bpp.ride (
      id, booking_id, driver_id, merchant_id, merchant_operating_city_id,
      chargeable_distance, fare, fare_parameters_id,
      otp, short_id, status, tracking_url, traveled_distance,
      trip_start_lat, trip_start_lon, trip_end_lat, trip_end_lon,
      trip_start_time, trip_end_time, created_at, updated_at
    ) VALUES (
      rid_completed, bid_completed::bpchar(36), ride_tpl.driver_id,
      COALESCE(ride_tpl.merchant_id, merchant_id_val::bpchar(36)),
      COALESCE(ride_tpl.merchant_operating_city_id, moc_id_val::bpchar(36)),
      ride_tpl.chargeable_distance, ride_tpl.fare, ride_tpl.fare_parameters_id,
      '1234', gen_random_uuid()::character varying(36), 'COMPLETED',
      COALESCE(ride_tpl.tracking_url, 'http://localhost/recon-ride/' || rid_completed),
      COALESCE(ride_tpl.traveled_distance, 0::double precision),
      ride_tpl.trip_start_lat, ride_tpl.trip_start_lon, ride_tpl.trip_end_lat, ride_tpl.trip_end_lon,
      COALESCE(ride_tpl.trip_start_time, day_mid), COALESCE(ride_tpl.trip_end_time, day_mid),
      now(), now()
    );

    INSERT INTO atlas_driver_offer_bpp.ride (
      id, booking_id, driver_id, merchant_id, merchant_operating_city_id,
      chargeable_distance, fare, fare_parameters_id,
      otp, short_id, status, tracking_url, traveled_distance,
      trip_start_lat, trip_start_lon, trip_end_lat, trip_end_lon,
      trip_start_time, trip_end_time, created_at, updated_at
    ) VALUES (
      rid_cancelled, bid_cancelled::bpchar(36), ride_tpl.driver_id,
      COALESCE(ride_tpl.merchant_id, merchant_id_val::bpchar(36)),
      COALESCE(ride_tpl.merchant_operating_city_id, moc_id_val::bpchar(36)),
      ride_tpl.chargeable_distance, ride_tpl.fare, ride_tpl.fare_parameters_id,
      '5678', gen_random_uuid()::character varying(36), 'CANCELLED',
      COALESCE(ride_tpl.tracking_url, 'http://localhost/recon-ride/' || rid_cancelled),
      COALESCE(ride_tpl.traveled_distance, 0::double precision),
      ride_tpl.trip_start_lat, ride_tpl.trip_start_lon, ride_tpl.trip_end_lat, ride_tpl.trip_end_lon,
      COALESCE(ride_tpl.trip_start_time, day_mid), COALESCE(ride_tpl.trip_end_time, day_mid),
      now(), now()
    );
  ELSE
    INSERT INTO atlas_driver_offer_bpp.ride (
      id, booking_id, driver_id, merchant_id, merchant_operating_city_id,
      fare_parameters_id,
      otp, short_id, status, tracking_url, traveled_distance,
      trip_start_lat, trip_start_lon, trip_end_lat, trip_end_lon,
      trip_start_time, trip_end_time, created_at, updated_at
    ) VALUES (
      rid_completed, bid_completed::bpchar(36), ride_driver_id::bpchar(36),
      merchant_id_val::bpchar(36), moc_id_val::bpchar(36),
      tpl.fare_parameters_id::bpchar(36),
      '1234', gen_random_uuid()::character varying(36), 'COMPLETED',
      'http://localhost/recon-ride/' || rid_completed, 1000::double precision,
      lat_from, lon_from, lat_to, lon_to,
      day_mid, day_mid, now(), now()
    );

    INSERT INTO atlas_driver_offer_bpp.ride (
      id, booking_id, driver_id, merchant_id, merchant_operating_city_id,
      fare_parameters_id,
      otp, short_id, status, tracking_url, traveled_distance,
      trip_start_lat, trip_start_lon, trip_end_lat, trip_end_lon,
      trip_start_time, trip_end_time, created_at, updated_at
    ) VALUES (
      rid_cancelled, bid_cancelled::bpchar(36), ride_driver_id::bpchar(36),
      merchant_id_val::bpchar(36), moc_id_val::bpchar(36),
      tpl.fare_parameters_id::bpchar(36),
      '5678', gen_random_uuid()::character varying(36), 'CANCELLED',
      'http://localhost/recon-ride/' || rid_cancelled, 0::double precision,
      lat_from, lon_from, lat_to, lon_to,
      day_mid, day_mid, now(), now()
    );
  END IF;

  RAISE NOTICE 'Location mock: pickup % drop % | rides % (completed) % (cancelled)', loc_from_id, loc_to_id, rid_completed, rid_cancelled;

  INSERT INTO atlas_driver_offer_bpp.finance_ledger_entry (
    id, amount, currency, entry_type, status, reference_type, reference_id,
    from_account_id, to_account_id, timestamp, merchant_id, merchant_operating_city_id,
    created_at, updated_at
  ) VALUES (
    ledger_booking_id, 10, 'INR', 'Revenue', 'SETTLED', 'Booking', bid_completed,
    acc_from_id, acc_to_id, day_mid, merchant_id_val, moc_id_val, now(), now()
  );

  RAISE NOTICE 'DSR_VS_LEDGER mock: bookings %, % | ledger %', bid_completed, bid_cancelled, ledger_booking_id;

  -- Driver + plan in same city (subscription + DSSR + DSR vs sub + PG payment + payout entity)
  -- driver_id_val already set from ride or person above; ensure plan lookup still works
  IF driver_id_val IS NOT NULL THEN
    SELECT pl.* INTO plan_rec
    FROM atlas_driver_offer_bpp.plan pl
    WHERE pl.merchant_op_city_id = moc_id_val
      AND pl.is_deprecated = false
    ORDER BY pl.id
    LIMIT 1;
    plan_ok := FOUND;
  END IF;

  IF driver_id_val IS NOT NULL AND plan_ok THEN
    sub_merchant_id := COALESCE(NULLIF(plan_rec.merchant_id, ''), merchant_id_val);

    plan_fee_val := GREATEST(
      COALESCE(plan_rec.registration_amount, 0),
      COALESCE(plan_rec.max_amount, 0),
      1
    )::double precision;

    INSERT INTO atlas_driver_offer_bpp.subscription_purchase (
      id, merchant_id, merchant_operating_city_id, owner_id, owner_type, payment_order_id,
      plan_fee, plan_frequency, plan_id, plan_ride_credit, purchase_timestamp, status,
      service_name, waive_of_mode, waiver_off_percentage, enable_service_usage_charge,
      created_at, updated_at
    ) VALUES (
      sub_id, sub_merchant_id, moc_id_val, driver_id_val, 'DRIVER', pay_order_id_val,
      plan_fee_val, plan_rec.frequency, plan_rec.id, plan_rec.max_credit_limit::double precision,
      day_mid, 'ACTIVE', 'PREPAID_SUBSCRIPTION', 'NO_WAIVE_OFF', 0, true, now(), now()
    );

    INSERT INTO atlas_driver_offer_bpp.finance_ledger_entry (
      id, amount, currency, entry_type, status, reference_type, reference_id,
      from_account_id, to_account_id, timestamp, merchant_id, merchant_operating_city_id,
      created_at, updated_at
    ) VALUES (
      ledger_sub_id, plan_fee_val, 'INR', 'Revenue', 'SETTLED', 'SubscriptionPurchase', sub_id,
      acc_from_id, acc_to_id, day_mid, sub_merchant_id, moc_id_val, now(), now()
    );

    INSERT INTO atlas_driver_offer_bpp.pg_payment_settlement_report (
      id, order_id, txn_id, rrn, utr, bank_id, txn_type, txn_status, txn_date,
      txn_amount, pg_base_fee, pg_tax, settlement_amount, currency,
      payment_gateway, pg_approval_code, payment_method, payment_method_sub_type,
      settlement_type, settlement_mode, settlement_id, settlement_date,
      reference_id, reference_type, recon_status, merchant_id,
      merchant_operating_city_id, created_at, updated_at
    ) VALUES (
      pgpay_id, pay_order_id_val, 'nammayatri-' || pay_order_id_val || '-1',
      'RRN123456789', 'UTR123456789', 'ICIC0001234',
      'ORDER', 'SUCCESS', day_mid,
      plan_fee_val, 0, 0, plan_fee_val, 'INR',
      'BILLDESK', 'APR001', 'UPI', 'UPI',
      'CREDIT', 'NET', settlement_id_val, settlement_ts,
      sub_id, 'SUBSCRIPTION_PURCHASE', 'PENDING', sub_merchant_id, moc_id_val, now(), now()
    );

    INSERT INTO atlas_driver_offer_bpp.payout_request (
      id, beneficiary_id, entity_id, entity_name, status, merchant_id, merchant_operating_city_id,
      amount, created_at, updated_at
    ) VALUES (
      payout_id, 'mock-bene-recon', driver_id_val, 'MANUAL', 'CREDITED',
      sub_merchant_id, moc_id_val, 100, day_mid, now()
    );

    INSERT INTO atlas_driver_offer_bpp.pg_payout_settlement_report (
      id, order_id, payout_customer_id, txn_status, txn_date, txn_amount, settlement_amount, currency,
      merchant_id, merchant_operating_city_id, payout_request_id, recon_status,
      created_at, updated_at
    ) VALUES (
      ppsr_id, payout_order_id, driver_id_val, 'SUCCESS', day_mid, 100, 100, 'INR',
      sub_merchant_id, moc_id_val, payout_id, 'PENDING', now(), now()
    );

    RAISE NOTICE 'Subscription mock: sub_id=% payment_order_id=% (use sub_id in dashboard seed / PG recon)', sub_id, pay_order_id_val;
    RAISE NOTICE 'DSSR / DSR_VS_SUBSCRIPTION / PG payment / payout mock rows inserted.';
  ELSE
    RAISE NOTICE 'Skipping subscription/PG/payout mocks: driver_id=% plan_ok=% (need plan for merchant_op_city_id %)', driver_id_val, plan_ok, moc_id_val;
  END IF;

  RAISE NOTICE 'Recon window used: % .. % (pick this day in dashboard)', day_start, day_end;
END $$;

COMMIT;

-- ---------------------------------------------------------------------------
-- After run: GET /api/finance/readiness with that calendar day + merchantOperatingCityId
-- ---------------------------------------------------------------------------
