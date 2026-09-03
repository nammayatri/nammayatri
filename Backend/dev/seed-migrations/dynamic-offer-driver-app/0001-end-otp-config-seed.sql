-- Seed one row per existing city for every (tripCategory, tripMode) combination that
-- TripCategory's Show/Read instances actually produce (see Trip.hs), with
-- is_end_otp_required set to match the current hardcoded isEndOtpRequired logic exactly,
-- so switching call sites over to this table changes nothing until someone edits a row.
-- ON CONFLICT DO NOTHING makes the seed itself safe to run twice too.
INSERT INTO atlas_driver_offer_bpp.end_otp_config
  (merchant_id, merchant_operating_city_id, trip_category, trip_mode, is_end_otp_required, created_at, updated_at)
SELECT
  moc.merchant_id,
  moc.id,
  x.trip_category,
  x.trip_mode,
  x.is_end_otp_required,
  CURRENT_TIMESTAMP,
  CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.merchant_operating_city moc
CROSS JOIN (
  VALUES
    ('Rental', 'RideOtp', true),
    ('Rental', 'OnDemandStaticOffer', true),
    ('InterCity', 'OneWayRideOtp', true),
    ('InterCity', 'OneWayOnDemandStaticOffer', true),
    ('InterCity', 'OneWayOnDemandDynamicOffer', true),
    ('Delivery', 'OneWayRideOtp', true),
    ('Delivery', 'OneWayOnDemandStaticOffer', true),
    ('Delivery', 'OneWayOnDemandDynamicOffer', true),
    ('EasyBooking', 'RideOtp', false),
    ('EasyBooking', 'OnDemandStaticOffer', false),
    ('OneWay', 'OneWayRideOtp', false),
    ('OneWay', 'OneWayOnDemandStaticOffer', false),
    ('OneWay', 'OneWayOnDemandDynamicOffer', false),
    ('RideShare', 'RideOtp', false),
    ('RideShare', 'OnDemandStaticOffer', false),
    ('CrossCity', 'OneWayRideOtp', false),
    ('CrossCity', 'OneWayOnDemandStaticOffer', false),
    ('CrossCity', 'OneWayOnDemandDynamicOffer', false),
    ('Ambulance', 'OneWayRideOtp', false),
    ('Ambulance', 'OneWayOnDemandStaticOffer', false),
    ('Ambulance', 'OneWayOnDemandDynamicOffer', false),
    -- OneWayMode also has a 4th constructor, MeterRide, excluded from Trip.hs's own
    -- generateTripCategoryShowInstances list (not exposed via the API schema) but still a
    -- valid TripCategory value, so it needs a row too for every category built on OneWayMode.
    ('OneWay', 'MeterRide', false),
    ('InterCity', 'MeterRide', true),
    ('CrossCity', 'MeterRide', false),
    ('Ambulance', 'MeterRide', false),
    ('Delivery', 'MeterRide', true)
) AS x(trip_category, trip_mode, is_end_otp_required)
ON CONFLICT (merchant_operating_city_id, trip_category, trip_mode) DO NOTHING;
