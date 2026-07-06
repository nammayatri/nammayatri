-- Helsinki (BRIDGE_FINLAND) — enable OTP-less rides on the rider-app RiderConfig.
--
-- Gates whether Person.enableOtpLessRide can be set to true (Registration + Profile)
-- and whether the Booking.enableOtpLessRide snapshot is taken at confirm time.
-- Ride-side reads pull directly from Booking; no runtime RiderConfig lookup.

DO $$
DECLARE
  v_city_id TEXT;
BEGIN
  SELECT moc.id INTO v_city_id
  FROM atlas_app.merchant_operating_city moc
  WHERE moc.merchant_short_id = 'BRIDGE_FINLAND' AND moc.city = 'Helsinki'
  LIMIT 1;

  IF v_city_id IS NULL THEN
    RAISE NOTICE 'BRIDGE_FINLAND Helsinki not found in atlas_app.merchant_operating_city, skipping OTP-less enable';
    RETURN;
  END IF;

  UPDATE atlas_app.rider_config
  SET enable_otp_less_ride = true
  WHERE merchant_operating_city_id = v_city_id;

  RAISE NOTICE 'BRIDGE_FINLAND Helsinki enable_otp_less_ride=true applied for merchant_operating_city_id %', v_city_id;
END $$;
