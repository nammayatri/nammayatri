-- DEFAULT DYNAMIC FCMS --

-- DRIVER_QUOTE_INCOMING --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_QUOTE_INCOMING',
    'DRIVER_QUOTE_INCOMING',
    moc.merchant_id,
    moc.id,
    'New driver offers incoming!',
    'There are new driver offers! Check the app for details',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;

-- DRIVER_ASSIGNMENT --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_ASSIGNMENT',
    'DRIVER_ASSIGNMENT',
    moc.merchant_id,
    moc.id,
    'Driver assigned!',
    '{#driverName#} will be your driver for this trip.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;

-- TRIP_STARTED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIP_STARTED',
    'TRIP_STARTED',
    moc.merchant_id,
    moc.id,
    'Your {#serviceTierName#} ride has started!',
    'Your {#serviceTierName#} ride with {#driverName#} has started. Enjoy the ride!',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;

-- TRIP_FINISHED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIP_FINISHED',
    'TRIP_FINISHED',
    moc.merchant_id,
    moc.id,
    'Trip finished!',
    'Hope you enjoyed your trip with {#driverName#}. Total Fare {#totalFare#}',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;

-- EXPIRED_CASE --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'EXPIRED_CASE',
    'EXPIRED_CASE',
    moc.merchant_id,
    moc.id,
    'Ride expired!',
    'Your ride has expired as you did not confirm any offer. Please book again to continue.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;

-- REGISTRATION_APPROVED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'REGISTRATION_APPROVED',
    'REGISTRATION_APPROVED',
    moc.merchant_id,
    moc.id,
    'Registration Completed!',
    'Welcome to Yatri. Click here to book your first ride with us.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;


-- BOOKING_CANCEL --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, fcm_sub_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'CANCELLED_PRODUCT',
    k.key,
    s.subcategory,
    moc.merchant_id,
    moc.id,
    CASE
        WHEN k.key = 'BOOKING_CANCEL_WITH_RIDE' THEN 'Ride cancelled!'
        WHEN k.key = 'BOOKING_CANCEL_WITH_NO_RIDE' AND s.subcategory = 'ByUser' THEN 'Ride cancelled!'
        WHEN k.key = 'BOOKING_CANCEL_WITH_NO_RIDE' AND s.subcategory = 'ByMerchant' THEN 'Ride Unavailable!'
        WHEN k.key = 'BOOKING_CANCEL_WITH_NO_RIDE' AND s.subcategory = 'ByDriver' THEN 'Ride Unavailable!'
        WHEN k.key = 'BOOKING_CANCEL_WITH_NO_RIDE' AND s.subcategory = 'ByAllocator' THEN 'Ride Unavailable!'
        WHEN k.key = 'BOOKING_CANCEL_WITH_NO_RIDE' AND s.subcategory = 'ByApplication' THEN 'Ride Unavailable!'
    END AS title,
    CASE
        WHEN s.subcategory = 'ByUser' THEN 'You have cancelled your ride for {#bookingStartTime#}. Check the app for details.'
        WHEN s.subcategory = 'ByMerchant' THEN '"{#orgName#}" agency had to cancel the ride for {#bookingStartTime#}. Please book again to get another ride.'
        WHEN k.key = 'BOOKING_CANCEL_WITH_RIDE' AND s.subcategory = 'ByDriver' THEN 'The driver had to cancel the ride for {#bookingStartTime#}. Please book again to get another ride.'
        WHEN k.key = 'BOOKING_CANCEL_WITH_NO_RIDE' AND s.subcategory = 'ByDriver' THEN 'Sorry, we could not find any driver for your ride at {#bookingStartTime#}. Please try to book again.'
        WHEN s.subcategory = 'ByAllocator' THEN 'The ride for {#bookingStartTime#} was cancelled as we could not find a driver. Please book again to get another ride.'
        WHEN k.key = 'BOOKING_CANCEL_WITH_RIDE' AND s.subcategory = 'ByApplication' THEN 'Sorry your ride for {#bookingStartTime#} was cancelled. Please try to book again.'
        WHEN k.key = 'BOOKING_CANCEL_WITH_NO_RIDE' AND s.subcategory = 'ByApplication' THEN 'Sorry, we could not find any driver for your ride at {#bookingStartTime#}. Please try to book again.'
    END AS body,
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
CROSS JOIN LATERAL unnest(array['BOOKING_CANCEL_WITH_RIDE', 'BOOKING_CANCEL_WITH_NO_RIDE']) as k(key)
CROSS JOIN LATERAL unnest(array['ByUser', 'ByMerchant', 'ByDriver', 'ByAllocator', 'ByApplication']) AS s(subcategory)
ON CONFLICT DO NOTHING;


-- BOOKING REALLOCATED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'REALLOCATE_PRODUCT',
    'BOOKING_REALLOCATED',
    moc.merchant_id,
    moc.id,
    'Ride cancelled! We are allocating another driver',
    'The driver had to cancel the ride for {#bookingStartTime#}. Please wait until we allocate another driver.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;

-- EST_OR_QUOTE_REALLOCATED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, fcm_sub_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'REALLOCATE_PRODUCT',
    'EST_OR_QUOTE_REALLOCATED',
    s.subcategory,
    moc.merchant_id,
    moc.id,
    'Searching for a New Driver!',
    CASE
        WHEN s.subcategory = 'ByUser' THEN 'You have cancelled your ride for {#bookingStartTime#}. Please wait while we allocate you another driver.'
        WHEN s.subcategory = 'ByMerchant' THEN 'The ride for {#bookingStartTime#}, is cancelled. Please wait while we allocate you another driver.'
        WHEN s.subcategory = 'ByDriver' THEN 'The driver had cancelled the ride for {#bookingStartTime#}. Please wait while we allocate you another driver.'
        WHEN s.subcategory = 'ByAllocator' THEN 'The ride for {#bookingStartTime#}, is cancelled. Please wait while we allocate you another driver.'
        WHEN s.subcategory = 'ByApplication' THEN 'Sorry your ride for  {#bookingStartTime#} was cancelled. Please wait while we allocate you another driver.'
    END AS body,
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
CROSS JOIN LATERAL unnest(array['ByUser', 'ByMerchant', 'ByDriver', 'ByAllocator', 'ByApplication']) AS s(subcategory)
ON CONFLICT DO NOTHING;



-- ON_QUOTE_RECEIVED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'QUOTE_RECEIVED',
    'QUOTE_RECEIVED',
    moc.merchant_id,
    moc.id,
    'Quote received!',
    'New quote received with price {#quoteFareEstimate#}',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;


-- DRIVER_ON_THE_WAY --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_ON_THE_WAY',
    'DRIVER_ON_THE_WAY',
    moc.merchant_id,
    moc.id,
    'Driver On The Way!',
    'Driver is on the way',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

-- DRIVER_HAS_REACHED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_HAS_REACHED',
    'DRIVER_HAS_REACHED',
    moc.merchant_id,
    moc.id,
    'Driver Has Reached!',
    'Use OTP {#otp#} to verify the ride with Vehicle No. {#vehicleNumber#}',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;


-- DRIVER_REACHING --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_REACHING',
    'DRIVER_REACHING',
    moc.merchant_id,
    moc.id,
    'Driver Arriving Now!',
    'Your driver is arriving now! Please be at the pickup location',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

-- SAFETY_ALERT_DEVIATION --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'SAFETY_ALERT_DEVIATION',
    'SAFETY_ALERT_DEVIATION',
    moc.merchant_id,
    moc.id,
    'Everything okay?',
    'We noticed your ride is on a different route. Are you feeling safe on your trip?',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;


-- DRIVER_BIRTHDAY --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_BIRTHDAY',
    'DRIVER_BIRTHDAY',
    moc.merchant_id,
    moc.id,
    'Driver''s Birthday!',
    'Today is your driver {#driverName#}''s birthday, your warm wishes will make their day even more special!',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;


-- FOLLOW RIDE --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FOLLOW_RIDE',
    'FOLLOW_RIDE',
    moc.merchant_id,
    moc.id,
    'Follow Ride',
    '{#name#} wants you to follow their ride',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;


-- STOP_REACHED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'STOP_REACHED',
    'STOP_REACHED',
    moc.merchant_id,
    moc.id,
    'Stop Reached!',
    '{#driverName#} has reached the stop. You may add another stop!',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;


-- TICKET_CANCELLED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'CANCELLED_PRODUCT',
    'TICKET_CANCELLED',
    moc.merchant_id,
    moc.id,
    '{#ticketBookingCategoryName#} Ticket Service is Cancelled',
    'Sorry, Ticket Booking {#ticketBookingId#} having {#ticketBookingCategoryName#} Service is cancelled will be Refunded. Check the app for details.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;


-- FIRST_RIDE_EVENT --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FIRST_RIDE_EVENT',
    'FIRST_RIDE_EVENT',
    moc.merchant_id,
    moc.id,
    'First Ride Event',
    'Congratulations! You have taken your first ride with us.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;

-- TRIP_UPDATED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIP_UPDATED',
    'TRIP_UPDATED',
    moc.merchant_id,
    moc.id,
    'Destination and Fare Updated',
    'Your edit request was accepted by your driver!',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

