-- only for PROD

INSERT INTO atlas_app.ride_related_notification_config (id, merchant_id, merchant_operating_city_id, time_diff, time_diff_event, notification_type, notification_key, on_booking_status, on_scheduled_booking, event_time)
SELECT
    atlas_app.uuid_generate_v4(),
    moc.merchant_id,
    moc.id,
    2700,
    'PICKUP_TIME',
    'SMS',
    'SEND_SCHEDULED_RIDE_DETAILS',
    'TRIP_ASSIGNED',
    True,
    'PreEvent'
FROM
    atlas_app.merchant_operating_city moc;

INSERT INTO atlas_app.ride_related_notification_config (id, merchant_id, merchant_operating_city_id, time_diff, time_diff_event, notification_type, notification_key, on_booking_status, on_scheduled_booking, event_time)
SELECT
    atlas_app.uuid_generate_v4(),
    moc.merchant_id,
    moc.id,
    60,
    'START_TIME',
    'SMS',
    'SCHEDULED_RIDE_OTP',
    'TRIP_ASSIGNED',
    True,
    'PostEvent'
FROM
    atlas_app.merchant_operating_city moc;

INSERT INTO atlas_app.ride_related_notification_config (id, merchant_id, merchant_operating_city_id, time_diff, time_diff_event, notification_type, notification_key, on_booking_status, on_scheduled_booking, event_time)
SELECT
    atlas_app.uuid_generate_v4(),
    moc.merchant_id,
    moc.id,
    2700,
    'PICKUP_TIME',
    'PN',
    'RIDE_START_END_OTP',
    'TRIP_ASSIGNED',
    True,
    'PreEvent'
FROM
    atlas_app.merchant_operating_city moc;

INSERT INTO atlas_app.merchant_push_notification (key, merchant_id, merchant_operating_city_id, title, body, language, fcm_notification_type)
SELECT
    'RIDE_START_END_OTP', moc.merchant_id, moc.id, 'Ride Start OTP', 'Dear User, Your ride start OTP is {#rideStartOtp#}. The driver will reach 15 minutes prior to the scheduled start time.', 'ENGLISH', 'SCHEDULED_RIDE_NOTIFICATION'
FROM
    atlas_app.merchant_operating_city moc;


INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id, sender_header)
SELECT
    moc.merchant_id,
    'SEND_SCHEDULED_RIDE_DETAILS',
    'Dear User, Your ride start OTP is {#rideStartOtp#}. The driver will reach 15 minutes prior to the scheduled start time -Namma Yatri',
    moc.id, 'NMYTRI'
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message, merchant_operating_city_id, sender_header)
SELECT
    moc.merchant_id,
    'SCHEDULED_RIDE_OTP',
    'Dear User, Your ride end OTP is {#rideEndOtp#}. Please use this to end your ride. Thank you for choosing us -Namma Yatri',
    moc.id, 'NMYTRI'
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;
