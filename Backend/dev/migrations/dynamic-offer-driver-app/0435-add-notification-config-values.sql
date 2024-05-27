INSERT INTO atlas_driver_offer_bpp.ride_related_notification_config (
    id, merchant_id, merchant_operating_city_id, time_diff, time_diff_event, notification_type, notification_key, on_booking_status, on_scheduled_booking, only_if_offline, event_time
    )
VALUES
(atlas_driver_offer_bpp.uuid_generate_v4(), '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f','1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', 2700, 'PICKUP_TIME', 'SMS', 'SCHEDULED_RIDE_REMINDER' , 'TRIP_ASSIGNED', True, True, 'PreEvent'),
(atlas_driver_offer_bpp.uuid_generate_v4(), '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f','1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', 2400, 'PICKUP_TIME', 'CALL', 'SCHEDULED_RIDE_REMINDER' , 'TRIP_ASSIGNED', True, True, 'PreEvent'),
(atlas_driver_offer_bpp.uuid_generate_v4(), '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f','1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', 0, 'RIDE_ASSIGNED', 'OVERLAY', 'RIDE_SCHEDULED' , 'TRIP_ASSIGNED', True, False, 'PreEvent'),
(atlas_driver_offer_bpp.uuid_generate_v4(), '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f','1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', 3600, 'PICKUP_TIME', 'OVERLAY', 'RIDE_SCHEDULED_REMINDER' , 'TRIP_ASSIGNED', True, False, 'PreEvent'),

(atlas_driver_offer_bpp.uuid_generate_v4(), '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f','1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', 3600, 'PICKUP_TIME', 'PN', 'RIDE_START_IN_ONE_HOURS' , 'TRIP_ASSIGNED', True, False, 'PreEvent'),
(atlas_driver_offer_bpp.uuid_generate_v4(), '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f','1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', 7200, 'PICKUP_TIME', 'PN', 'RIDE_START_IN_TWO_HOURS' , 'TRIP_ASSIGNED', True, False, 'PreEvent'),
(atlas_driver_offer_bpp.uuid_generate_v4(), '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f','1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', 3600, 'PICKUP_TIME', 'PN', 'SCHEDULED_RIDE_ASSIGNED' , 'TRIP_ASSIGNED', True, False, 'PreEvent')
(atlas_driver_offer_bpp.uuid_generate_v4(), '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f','1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', 2700, 'PICKUP_TIME', 'PN', 'GO_ONLINE_REMINDER' , 'TRIP_ASSIGNED', True, True, 'PreEvent');



INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message)
VALUES ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'SCHEDULED_RIDE_REMINDER', 'You have an assigned ride starting in 45 minutes. Please go Online on Namma Yatri driver partner app and start moving to the pickup location.');

INSERT INTO atlas_driver_offer_bpp.merchant_overlay (id, merchant_id, language, overlay_key, udf1, title, description, ok_button_text)
VALUES
(atlas_driver_offer_bpp.uuid_generate_v4(), '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'RIDE_SCHEDULED', 'Z9', '{#IsRentalOrIntercity#} Ride Allocated', 'Reach Customer pickup location before 15 minutes from scheduled ride start time\n Be online before 45 minutes of Scheduled Ride Start\n Cancellation before 1 hr from the scheduled ride start will lead to penalties on the platform.\n Sakkath Service ! Namma Duty !', 'Okay'),
(atlas_driver_offer_bpp.uuid_generate_v4(), '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'RIDE_SCHEDULED_REMINDER', 'Z9', 'Scheduled {#IsRentalOrIntercity#} Ride Allocated', 'Reach Customer pickup location before 15 minutes of ride start time\n Be online before 45 minutes of Scheduled Ride Start\n Cancellation before 1 hr from the scheduled ride start will lead to penalties on the platform.\n Sakkath Service ! Namma Duty !', 'Okay'),

INSERT INTO atlas_driver_offer_bpp.merchant_push_notifications (key, merchantId, merchantOperatingCityId, title, body, language, fcmNotificationType)
VALUES
( 'RIDE_START_IN_TWO_HOURS', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', '{#IsRentalOrIntercity#} Ride starts in 2 hrs.', 'Pickup at {#pickupAddress#}', 'ENGLISH', 'SCHEDULED_RIDE_NOTIFICATION'),
( 'RIDE_START_IN_ONE_HOURS', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', '{#IsRentalOrIntercity#} Ride starts in 1 hrs.', 'Pickup at {#pickupAddress#}', 'ENGLISH', 'SCHEDULED_RIDE_NOTIFICATION'),
( 'SCHEDULED_RIDE_ASSIGNED', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', 'Ride Assinged', '{#IsRentalOrIntercity#} ride is now active', 'ENGLISH', 'SCHEDULED_RIDE_NOTIFICATION'),
( 'GO_ONLINE_REMINDER', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f','1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', 'Please Go Online' , 'You have an assigned ride starting in 45 minutes.\n Please go Online on Namma Yatri driver partner app\n and start moving to the pickup location.', 'ENGLISH', 'SCHEDULED_RIDE_NOTIFICATION'),
( 'TOLL_CROSSED', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', 'Toll Crossed !', 'Toll charges added to final fare', 'ENGLISH', 'TOLL_CROSSED');