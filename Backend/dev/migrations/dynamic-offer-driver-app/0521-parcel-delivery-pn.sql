
-- NEW_RIDE_AVAILABLE --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'NEW_RIDE_AVAILABLE',
    'NEW_RIDE_AVAILABLE',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'New parcel delivery available for offering!',
    'A new delivery for {#startTime#} is available {#distanceToPickup#} away from you. Estimated base fare is {#baseFare#} INR, estimated distance is {#distance#}',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;


-- NOTIFY_DRIVER_ON_CANCEL --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, fcm_sub_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'CANCELLED_PRODUCT',
    'NOTIFY_DRIVER_ON_CANCEL',
    'Delivery_OneWayOnDemandDynamicOffer',
    s.subcategory,
    moc.merchant_id,
    moc.id,
    'Ride cancelled!',
    CASE
        WHEN s.subcategory = 'ByUser' THEN 'Customer had to cancel the delivery scheduled for {#startTime#}. Check the app for more details.'
        WHEN s.subcategory = 'ByMerchant' THEN 'Your agency had to cancel the delivery scheduled for {#startTime#}. Check the app for more details.'
        WHEN s.subcategory = 'ByDriver' THEN 'You have cancelled the delivery scheduled for {#startTime#}. Check the app for more details.'
        WHEN s.subcategory = 'ByApplication' THEN 'Sorry your delivery for {#startTime#} was cancelled. Please try again.'
    END AS body,
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc
CROSS JOIN LATERAL unnest(array['ByUser', 'ByMerchant', 'ByDriver', 'ByApplication']) AS s(subcategory);

-- ALLOCATION_REQUEST --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'ALLOCATION_REQUEST',
    'ALLOCATION_REQUEST',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'New parcel delivery request',
    'You have a new delivery request! Check the app for details.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

-- CANCELLED_SEARCH_REQUEST --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'CANCELLED_SEARCH_REQUEST',
    'CANCELLED_SEARCH_REQUEST',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Delivery Request cancelled!',
    'The delivery request has been canceled by the customer.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;


-- RIDE_STARTED --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIP_STARTED',
    'RIDE_STARTED',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Delivery started',
    'The delivery has started. Please provide {#offerAdjective#} service and and drive safely!',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;
