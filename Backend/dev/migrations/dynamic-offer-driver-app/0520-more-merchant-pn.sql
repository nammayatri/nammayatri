
-- NEW_RIDE_AVAILABLE --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'NEW_RIDE_AVAILABLE',
    'NEW_RIDE_AVAILABLE',
    moc.merchant_id,
    moc.id,
    'New ride available for offering',
    'A new ride for {#startTime#} is available {#distanceToPickup#} away from you. Estimated base fare is {#baseFare#} INR, estimated distance is {#distance#}',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;


-- NOTIFY_DRIVER_ON_CANCEL --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, fcm_sub_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'CANCELLED_PRODUCT',
    'NOTIFY_DRIVER_ON_CANCEL',
    s.subcategory,
    moc.merchant_id,
    moc.id,
    'Ride cancelled!',
    CASE
        WHEN s.subcategory = 'ByUser' THEN 'Customer had to cancel your ride for {#startTime#}. Check the app for more details.'
        WHEN s.subcategory = 'ByMerchant' THEN 'Your agency had to cancel the ride for {#startTime#}. Check the app for more details.'
        WHEN s.subcategory = 'ByDriver' THEN 'You have cancelled the ride for {#startTime#}. Check the app for more details.'
        WHEN s.subcategory = 'ByApplication' THEN 'Sorry your ride for {#startTime#} was cancelled. Please try to book again'
    END AS body,
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc
CROSS JOIN LATERAL unnest(array['ByUser', 'ByMerchant', 'ByDriver', 'ByApplication']) AS s(subcategory);


-- REGISTRATION_APPROVED --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'REGISTRATION_APPROVED',
    'REGISTRATION_APPROVED',
    moc.merchant_id,
    moc.id,
    'Registration Completed!',
    'Welcome Yatri Partner! Click here to set up your account.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;


-- ALLOCATION_REQUEST --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'ALLOCATION_REQUEST',
    'ALLOCATION_REQUEST',
    moc.merchant_id,
    moc.id,
    'New allocation request.',
    'New ride request! Check the app for more details.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;


-- CLEARED_FARE --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'CLEARED_FARE',
    'CLEARED_FARE',
    moc.merchant_id,
    moc.id,
    'Clearing Fare!',
    'Clearing fare - {#amount#} {#currency#}.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;


-- CANCELLED_SEARCH_REQUEST --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'CANCELLED_SEARCH_REQUEST',
    'CANCELLED_SEARCH_REQUEST',
    moc.merchant_id,
    moc.id,
    'Search Request cancelled!',
    'Search request has been cancelled by customer',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;


-- PAYMENT_FAILED --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYMENT_FAILED',
    'PAYMENT_FAILED',
    moc.merchant_id,
    moc.id,
    'Payment Failed!',
    'Your payment attempt was unsuccessful.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;


-- PAYMENT_PENDING --

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYMENT_PENDING',
    'PAYMENT_PENDING',
    moc.merchant_id,
    moc.id,
    'Payment Pending!',
    'To continue taking rides on Namma Yatri, clear you payment dues',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;


-- PAYMENT_SUCCESS --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYMENT_SUCCESS',
    'PAYMENT_SUCCESS',
    moc.merchant_id,
    moc.id,
    'Payment Successful!',
    'Your payment has been processed successfully. Start earning with Namma Yatri!',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;



-- PAYMENT_MODE_MANUAL_ON_CANCEL --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYMENT_MODE_MANUAL',
    'PAYMENT_MODE_MANUAL_ON_CANCEL',
    moc.merchant_id,
    moc.id,
    'Payment mode changed to manual',
    'You have cancelled your UPI Autopay. You can clear your dues manually from the Plan page.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;


-- PAYMENT_MODE_MANUAL_ON_PAUSE --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYMENT_MODE_MANUAL',
    'PAYMENT_MODE_MANUAL_ON_PAUSE',
    moc.merchant_id,
    moc.id,
    'Payment mode changed to manual',
    'You have paused your UPI Autopay. You can clear your dues manually from the Plan page.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

-- PAYMENT_MODE_MANUAL_ON_SUSPEND --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYMENT_MODE_MANUAL',
    'PAYMENT_MODE_MANUAL_ON_SUSPEND',
    moc.merchant_id,
    moc.id,
    'Payment mode changed to manual',
    'Your UPI Autopay has been suspended. You can clear your dues manually from the Plan page.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

-- EDIT_STOP/ADD_STOP --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    nt.key,
    nt.key,
    moc.merchant_id,
    moc.id,
    CASE
        WHEN nt.key = 'EDIT_STOP' THEN 'Stop Edited'
        WHEN nt.key = 'ADD_STOP' THEN 'Stop Added'
    END AS title,
    CASE
        WHEN nt.key = 'EDIT_STOP' THEN 'Customer edited stop!'
        WHEN nt.key = 'ADD_STOP' THEN 'Customer added a stop!'
    END AS body,
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc
CROSS JOIN LATERAL unnest(array['EDIT_STOP', 'ADD_STOP']) AS nt(key);




-- RIDE_STARTED --
-- 1. AC_RIDE_STARTED --
-- 2. RIDE_STARTED --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIP_STARTED',
    nt.key,
    moc.merchant_id,
    moc.id,
    CASE
        WHEN nt.key = 'AC_RIDE_STARTED' THEN 'Your AC ride has started'
        WHEN nt.key = 'RIDE_STARTED' THEN 'Your ride has started'
    END AS title,
    CASE
        WHEN nt.key = 'AC_RIDE_STARTED' THEN 'Please turn on AC, offer {#offerAdjective#} service and have a safe ride!'
        WHEN nt.key = 'RIDE_STARTED' THEN 'Offer {#offerAdjective#} service and have a safe ride!'
    END AS body,
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc
CROSS JOIN LATERAL unnest(array['AC_RIDE_STARTED', 'RIDE_STARTED']) AS nt(key);

