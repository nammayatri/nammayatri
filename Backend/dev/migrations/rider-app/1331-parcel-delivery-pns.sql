-- DEFAULT DYNAMIC FCMS --

-- DRIVER_QUOTE_INCOMING --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_QUOTE_INCOMING',
    'DRIVER_QUOTE_INCOMING',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'New delivery offers available!',
    'There are new driver offers for delivering your parcel. Check the app for details.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

-- DRIVER_ASSIGNMENT --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_ASSIGNMENT',
    'DRIVER_ASSIGNMENT',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Driver assigned to your parcel!',
    '{#driverName#} will be handling the delivery of your parcel.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

-- TRIP_STARTED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIP_STARTED',
    'TRIP_STARTED',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Your parcel is on the move!',
    'Your parcel with {#driverName#} is now en route. Track it live in the app!',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

-- TRIP_FINISHED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIP_FINISHED',
    'TRIP_FINISHED',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Parcel delivered!',
    'Your parcel has been delivered successfully by {#driverName#}. Total Fare {#totalFare#}',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

-- EXPIRED_CASE --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'EXPIRED_CASE',
    'EXPIRED_CASE',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Delivery request expired!',
    'Your delivery request expired as no offers were confirmed. Please book again to continue.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

-- BOOKING_CANCEL --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, fcm_sub_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'CANCELLED_PRODUCT',
    k.key,
    'Delivery_OneWayOnDemandDynamicOffer',
    s.subcategory,
    moc.merchant_id,
    moc.id,
    CASE
        WHEN k.key = 'BOOKING_CANCEL_WITH_RIDE' THEN 'Parcel delivery cancelled!'
        WHEN k.key = 'BOOKING_CANCEL_WITH_NO_RIDE' AND s.subcategory = 'ByUser' THEN 'Parcel delivery cancelled!'
        WHEN k.key = 'BOOKING_CANCEL_WITH_NO_RIDE' AND s.subcategory = 'ByMerchant' THEN 'Parcel delivery cancelled!'
        WHEN k.key = 'BOOKING_CANCEL_WITH_NO_RIDE' AND s.subcategory = 'ByDriver' THEN 'Parcel delivery cancelled!'
        WHEN k.key = 'BOOKING_CANCEL_WITH_NO_RIDE' AND s.subcategory = 'ByAllocator' THEN 'Parcel delivery cancelled!'
        WHEN k.key = 'BOOKING_CANCEL_WITH_NO_RIDE' AND s.subcategory = 'ByApplication' THEN 'Parcel delivery cancelled!'
    END AS title,
    CASE
        WHEN s.subcategory = 'ByUser' THEN 'You cancelled the parcel delivery scheduled for {#bookingStartTime#}. Check the app for details.'
        WHEN s.subcategory = 'ByMerchant' THEN '"{#orgName#}" agency had to cancel the parcel delivery scheduled for {#bookingStartTime#}. Please book again to get another ride.'
        WHEN k.key = 'BOOKING_CANCEL_WITH_RIDE' AND s.subcategory = 'ByDriver' THEN 'The driver had to cancel the parcel delivery scheduled for {#bookingStartTime#}. Please book again to get another ride.'
        WHEN k.key = 'BOOKING_CANCEL_WITH_NO_RIDE' AND s.subcategory = 'ByDriver' THEN 'Sorry, we could not find any driver for your parcel delivery at {#bookingStartTime#}. Please try to book again.'
        WHEN s.subcategory = 'ByAllocator' THEN 'The parcel delivery for {#bookingStartTime#} was cancelled as we could not find a driver. Please book again to get another ride.'
        WHEN k.key = 'BOOKING_CANCEL_WITH_RIDE' AND s.subcategory = 'ByApplication' THEN 'Sorry your parcel delivery for {#bookingStartTime#} was cancelled. Please try to book again.'
        WHEN k.key = 'BOOKING_CANCEL_WITH_NO_RIDE' AND s.subcategory = 'ByApplication' THEN 'Sorry, we could not find any driver for your parcel delivery at {#bookingStartTime#}. Please try to book again.'
    END AS body,
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
CROSS JOIN LATERAL unnest(array['BOOKING_CANCEL_WITH_RIDE', 'BOOKING_CANCEL_WITH_NO_RIDE']) as k(key)
CROSS JOIN LATERAL unnest(array['ByUser', 'ByMerchant', 'ByDriver', 'ByAllocator', 'ByApplication']) AS s(subcategory);


-- BOOKING REALLOCATED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'REALLOCATE_PRODUCT',
    'BOOKING_REALLOCATED',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Reallocating your parcel delivery!',
    'The driver had to cancel the parcel delivery for {#bookingStartTime#}. Please wait until we allocate another driver.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

-- EST_OR_QUOTE_REALLOCATED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, fcm_sub_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'REALLOCATE_PRODUCT',
    'EST_OR_QUOTE_REALLOCATED',
    'Delivery_OneWayOnDemandDynamicOffer',
    s.subcategory,
    moc.merchant_id,
    moc.id,
    'Searching for a New Driver!',
    CASE
        WHEN s.subcategory = 'ByUser' THEN 'You have cancelled your parcel delivery for {#bookingStartTime#}. Please wait while we allocate you another driver.'
        WHEN s.subcategory = 'ByMerchant' THEN 'The parcel delivery for {#bookingStartTime#}, is cancelled. Please wait while we allocate you another driver.'
        WHEN s.subcategory = 'ByDriver' THEN 'The driver had cancelled the parcel delivery for {#bookingStartTime#}. Please wait while we allocate you another driver.'
        WHEN s.subcategory = 'ByAllocator' THEN 'The parcel delivery for {#bookingStartTime#}, is cancelled. Please wait while we allocate you another driver.'
        WHEN s.subcategory = 'ByApplication' THEN 'Sorry your parcel delivery for {#bookingStartTime#} was cancelled. Please wait while we allocate you another driver.'
    END AS body,
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
CROSS JOIN LATERAL unnest(array['ByUser', 'ByMerchant', 'ByDriver', 'ByAllocator', 'ByApplication']) AS s(subcategory);



-- ON_QUOTE_RECEIVED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'QUOTE_RECEIVED',
    'QUOTE_RECEIVED',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Delivery quote received!',
    'You''ve received a new quote for delivering your parcel: {#quoteFareEstimate#}',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;


-- DRIVER_ON_THE_WAY --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_ON_THE_WAY',
    'DRIVER_ON_THE_WAY',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Driver On The Way!',
    'Driver is heading to the pickup location for your parcel.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

-- DRIVER_HAS_REACHED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_HAS_REACHED',
    'DRIVER_HAS_REACHED',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Driver at pickup location!',
    'Use OTP {#otp#} to verify the pickup with Vehicle No. {#vehicleNumber#}',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;


-- Driver has reached destination --
INSERT INTO atlas_app.notification_sounds_config
    (merchant_id, merchant_operating_city_id, notification_type, default_sound, blind_sound)
SELECT
    city.merchant_id,
    city.id,
    'DRIVER_HAS_REACHED_DESTINATION',
    'driver_arrived.mp3',
    'driver_arrived.mp3'
FROM atlas_app.merchant_operating_city AS city;

-- DRIVER_HAS_REACHED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_HAS_REACHED_DESTINATION',
    'DRIVER_HAS_REACHED_DESTINATION',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Driver reached destination!',
    'Driver has reached the parcel drop location',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;


-- DRIVER_REACHING --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_REACHING',
    'DRIVER_REACHING',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Driver Arriving Now!',
    'Your driver is almost at the pickup location. Please be ready.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

-- SAFETY_ALERT_DEVIATION --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'SAFETY_ALERT_DEVIATION',
    'SAFETY_ALERT_DEVIATION',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Parcel delivery safety alert!',
    'Your parcel''s delivery route has changed unexpectedly. Check the app for details.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;


-- DRIVER_BIRTHDAY --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_BIRTHDAY',
    'DRIVER_BIRTHDAY',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Driver''s Birthday!',
    'Today is your driver {#driverName#}''s birthday, wish them well when they deliver your parcel!',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;


-- FOLLOW RIDE --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FOLLOW_RIDE',
    'FOLLOW_RIDE',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Follow Parcel Delivery',
    '{#name#} wants you to follow their parcel delivery',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

-- FIRST_RIDE_EVENT --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FIRST_RIDE_EVENT',
    'FIRST_RIDE_EVENT',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'First Parcel Delivery',
    'Congratulations! You have done your first parcel delivery with us.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

--- FILE UPLOAD Sound Config --
INSERT INTO atlas_app.notification_sounds_config
    (merchant_id, merchant_operating_city_id, notification_type, default_sound, blind_sound)
SELECT
    city.merchant_id,
    city.id,
    'FILE_UPLOADED',
    'default',
    'default'
FROM atlas_app.merchant_operating_city AS city;


-- PARCEL_IMAGE_UPLOADED --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FILE_UPLOADED',
    'PARCEL_IMAGE_UPLOADED',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Parcel image uploaded',
    'Your parcel image is uploaded by driver. Please open app to see it.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;
