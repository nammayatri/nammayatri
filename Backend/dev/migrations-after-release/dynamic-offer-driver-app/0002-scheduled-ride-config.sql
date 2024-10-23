-- only for PROD

UPDATE atlas_driver_offer_bpp.transporter_config
SET exotel_app_id_mapping = '{"exotelMap":{"SosAppletID": "13775", "RentalAppletID": "849601"}}';

INSERT INTO atlas_driver_offer_bpp.ride_related_notification_config (id, merchant_id, merchant_operating_city_id, time_diff, time_diff_event, notification_type, notification_key, on_booking_status, on_scheduled_booking, only_if_offline, event_time)
SELECT
    atlas_driver_offer_bpp.uuid_generate_v4(),
    moc.merchant_id,
    moc.id,
    2400,
    'PICKUP_TIME',
    'CALL',
    'SCHEDULED_RIDE_REMINDER',
    'TRIP_ASSIGNED',
    True,
    True,
    'PreEvent'
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;


INSERT INTO atlas_driver_offer_bpp.ride_related_notification_config (id, merchant_id, merchant_operating_city_id, time_diff, time_diff_event, notification_type, notification_key, on_booking_status, on_scheduled_booking, only_if_offline, event_time)
SELECT
    atlas_driver_offer_bpp.uuid_generate_v4(),
    moc.merchant_id,
    moc.id,
    2700,
    'PICKUP_TIME',
    'SMS',
    'SMS_TO_GO_ONLINE_IN_SCHEDULED_RIDE',
    'TRIP_ASSIGNED',
    True,
    True,
    'PreEvent'
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;


INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message, merchant_operating_city_id, sender_header)
SELECT
    moc.merchant_id,
    'SMS_TO_GO_ONLINE_IN_SCHEDULED_RIDE',
    'You have an assigned ride starting in 45 minutes. Please go Online on {#driverPartnerName#} Partner app and start moving to the pickup location. -Namma Yatri',
    moc.id, 'NMYTRI'
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;
