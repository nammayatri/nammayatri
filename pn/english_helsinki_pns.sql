
INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FIRST_RIDE_EVENT','FIRST_RIDE_EVENT','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'First Parcel Delivery','Congratulations! You have done your first parcel delivery with us.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Searching for a New Driver!','The ride for {#bookingStartTime#}, is cancelled. Please wait while we allocate you another driver.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FCM_CHAT_MESSAGE','FCM_CHAT_MESSAGE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Missed Call: Quick Action Required','Your driver tried to contact you but couldn''t get through. Please call them back to coordinate your pickup.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'FIRST_RIDE_EVENT','FIRST_RIDE_EVENT',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  '🎉 Congrats on your 1st Ride with Bridge !','Your choice supports drivers directly and makes a real difference.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_QUOTE_INCOMING','DRIVER_QUOTE_INCOMING',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'New driver offers incoming!','There are new driver offers! Check the app for details','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ASSIGNMENT','DRIVER_ASSIGNMENT',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Driver assigned!','{#driverName#} will be your driver for this trip.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_STARTED','TRIP_STARTED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Your {#serviceTierName#} ride has started!','Your {#serviceTierName#} ride with {#driverName#} has started. Enjoy the ride!','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'EXPIRED_CASE','EXPIRED_CASE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Ride expired!','Your ride has expired as you did not confirm any offer. Please book again to continue.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REGISTRATION_APPROVED','REGISTRATION_APPROVED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Registration Completed!','Welcome to Yatri. Click here to book your first ride with us.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Ride cancelled!','You have cancelled your ride for {#bookingStartTime#}. Check the app for details.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Ride cancelled!','"{#orgName#}" agency had to cancel the ride for {#bookingStartTime#}. Please book again to get another ride.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Ride cancelled!','The driver had to cancel the ride for {#bookingStartTime#}. Please book again to get another ride.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Ride cancelled!','The ride for {#bookingStartTime#} was cancelled as we could not find a driver. Please book again to get another ride.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Ride cancelled!','Sorry your ride for {#bookingStartTime#} was cancelled. Please try to book again.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Ride cancelled!','You have cancelled your ride for {#bookingStartTime#}. Check the app for details.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Ride Unavailable!','"{#orgName#}" agency had to cancel the ride for {#bookingStartTime#}. Please book again to get another ride.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Ride Unavailable!','Sorry, we could not find any driver for your ride at {#bookingStartTime#}. Please try to book again.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Ride Unavailable!','The ride for {#bookingStartTime#} was cancelled as we could not find a driver. Please book again to get another ride.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Ride Unavailable!','Sorry, we could not find any driver for your ride at {#bookingStartTime#}. Please try to book again.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','BOOKING_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Ride cancelled! We are allocating another driver','The driver had to cancel the ride for {#bookingStartTime#}. Please wait until we allocate another driver.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Searching for a New Driver!','You have cancelled your ride for {#bookingStartTime#}. Please wait while we allocate you another driver.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Searching for a New Driver!','The driver had cancelled the ride for {#bookingStartTime#}. Please wait while we allocate you another driver.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Searching for a New Driver!','The ride for {#bookingStartTime#}, is cancelled. Please wait while we allocate you another driver.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Searching for a New Driver!','Sorry your ride for  {#bookingStartTime#} was cancelled. Please wait while we allocate you another driver.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'QUOTE_RECEIVED','QUOTE_RECEIVED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Quote received!','New quote received with price {#quoteFareEstimate#}','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ON_THE_WAY','DRIVER_ON_THE_WAY',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Driver On The Way!','Driver is on the way','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED','DRIVER_HAS_REACHED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Driver Has Reached!','Use OTP {#otp#} to verify the ride with Vehicle No. {#vehicleNumber#}','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_REACHING','DRIVER_REACHING',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Driver Arriving Now!','Your driver is arriving now! Please be at the pickup location','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_DEVIATION','SAFETY_ALERT_DEVIATION',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Everything okay?','We noticed your ride is on a different route. Are you feeling safe on your trip?','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_BIRTHDAY','DRIVER_BIRTHDAY',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Driver''s Birthday!','Today is your driver {#driverName#}''s birthday, your warm wishes will make their day even more special!','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FOLLOW_RIDE','FOLLOW_RIDE',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Follow Ride','{#name#} wants you to follow their ride','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'STOP_REACHED','STOP_REACHED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Stop Reached!','{#driverName#} has reached the stop. You may add another stop!','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','TICKET_CANCELLED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  '{#ticketBookingCategoryName#} Ticket Service is Cancelled','Sorry, Ticket Booking {#ticketBookingId#} having {#ticketBookingCategoryName#} Service is cancelled will be Refunded. Check the app for details.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_UPDATED','TRIP_UPDATED',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Destination and Fare Updated','Your edit request was accepted by your driver!','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

-- Delivery: Dynamic Offer Parcel Delivery variants --

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_QUOTE_INCOMING','DRIVER_QUOTE_INCOMING','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'New delivery offers available!','There are new driver offers for delivering your parcel. Check the app for details.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ASSIGNMENT','DRIVER_ASSIGNMENT','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Driver assigned to your parcel!','{#driverName#} will be handling the delivery of your parcel.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_STARTED','TRIP_STARTED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Your parcel is on the move!','Your parcel with {#driverName#} is now en route. Track it live in the app!','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'TRIP_FINISHED','TRIP_FINISHED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Parcel delivered!','Your parcel has been delivered successfully by {#driverName#}. Total Fare {#totalFare#}','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'EXPIRED_CASE','EXPIRED_CASE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'Delivery request expired!','Your delivery request expired as no offers were confirmed. Please book again to continue.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Parcel delivery cancelled!','You cancelled the parcel delivery scheduled for {#bookingStartTime#}. Check the app for details.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Parcel delivery cancelled!','"{#orgName#}" agency had to cancel the parcel delivery scheduled for {#bookingStartTime#}. Please book again to get another ride.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Parcel delivery cancelled!','The driver had to cancel the parcel delivery scheduled for {#bookingStartTime#}. Please book again to get another ride.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Parcel delivery cancelled!','The parcel delivery for {#bookingStartTime#} was cancelled as we could not find a driver. Please book again to get another ride.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Parcel delivery cancelled!','Sorry your parcel delivery for {#bookingStartTime#} was cancelled. Please try to book again.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Parcel delivery cancelled!','You cancelled the parcel delivery scheduled for {#bookingStartTime#}. Check the app for details.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Parcel delivery cancelled!','"{#orgName#}" agency had to cancel the parcel delivery scheduled for {#bookingStartTime#}. Please book again to get another ride.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Parcel delivery cancelled!','Sorry, we could not find any driver for your parcel delivery at {#bookingStartTime#}. Please try to book again.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Parcel delivery cancelled!','The parcel delivery for {#bookingStartTime#} was cancelled as we could not find a driver. Please book again to get another ride.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'CANCELLED_PRODUCT','BOOKING_CANCEL_WITH_NO_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Parcel delivery cancelled!','Sorry, we could not find any driver for your parcel delivery at {#bookingStartTime#}. Please try to book again.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','BOOKING_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Reallocating your parcel delivery!','The driver had to cancel the parcel delivery for {#bookingStartTime#}. Please wait until we allocate another driver.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByUser',
  'Searching for a New Driver!','You have cancelled your parcel delivery for {#bookingStartTime#}. Please wait while we allocate you another driver.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByMerchant',
  'Searching for a New Driver!','The parcel delivery for {#bookingStartTime#}, is cancelled. Please wait while we allocate you another driver.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByDriver',
  'Searching for a New Driver!','The driver had cancelled the parcel delivery for {#bookingStartTime#}. Please wait while we allocate you another driver.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByAllocator',
  'Searching for a New Driver!','The parcel delivery for {#bookingStartTime#}, is cancelled. Please wait while we allocate you another driver.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, fcm_sub_category, title, body, language, created_at, updated_at
) VALUES (
  'REALLOCATE_PRODUCT','EST_OR_QUOTE_REALLOCATED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'ByApplication',
  'Searching for a New Driver!','Sorry your parcel delivery for {#bookingStartTime#} was cancelled. Please wait while we allocate you another driver.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'QUOTE_RECEIVED','QUOTE_RECEIVED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Delivery quote received!','You''ve received a new quote for delivering your parcel: {#quoteFareEstimate#}','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_ON_THE_WAY','DRIVER_ON_THE_WAY','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Driver On The Way!','Driver is heading to the pickup location for your parcel.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED','DRIVER_HAS_REACHED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Driver at pickup location!','Use OTP {#otp#} to verify the pickup with Vehicle No. {#vehicleNumber#}','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_HAS_REACHED_DESTINATION','DRIVER_HAS_REACHED_DESTINATION','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Driver reached destination!','Driver has reached the parcel drop location','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_REACHING','DRIVER_REACHING','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Driver Arriving Now!','Your driver is almost at the pickup location. Please be ready.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_DEVIATION','SAFETY_ALERT_DEVIATION','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Parcel delivery safety alert!','Your parcel''s delivery route has changed unexpectedly. Check the app for details.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'DRIVER_BIRTHDAY','DRIVER_BIRTHDAY','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Driver''s Birthday!','Today is your driver {#driverName#}''s birthday, wish them well when they deliver your parcel!','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FOLLOW_RIDE','FOLLOW_RIDE','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Follow Parcel Delivery','{#name#} wants you to follow their parcel delivery','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
) VALUES (
  'FILE_UPLOADED','PARCEL_IMAGE_UPLOADED','Delivery_OneWayOnDemandDynamicOffer',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'Parcel image uploaded','Your parcel image is uploaded by driver. Please open app to see it.','ENGLISH',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'SCHEDULED_RIDE_NOTIFICATION','RIDE_START_REMINDER','{#isRentalOrIntercity#} Booking Reminder.','Ride starts at {#rideStartTime#}. Details will be shared shortly.','ENGLISH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'SCHEDULED_RIDE_NOTIFICATION','RIDE_START_END_OTP','Ride Start OTP','Dear User, Your ride start OTP is {#rideStartOtp#}. The driver will reach 15 minutes prior to the scheduled start time.','ENGLISH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'SAFETY_ALERT_RIDE_STOPPAGE','SAFETY_ALERT_RIDE_STOPPAGE','Everything okay?','We noticed your driver has not moved for a while. Are you feeling safe on your trip?','ENGLISH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRAL_REWARD_ADD_VPA','💸 Ka-ching! You Just Earned a Reward','Your friend''s first ride is complete! Add UPI ID and earn referral reward.','ENGLISH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRAL_REWARD','💸 Ka-ching! You Just Earned a Reward','Your friend''s first ride is complete! Keep the buzz going—refer more friends and earn for every new rider.','ENGLISH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRED_BY_REWARD_ADD_VPA','💸 Ka-ching! You Just Earned a Reward','Add UPI ID and earn the reward. Thanks for using Bridge, Keep supporting our drivers !','ENGLISH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRAL_BONUS_EARNED','💸 Ka-ching! You Just Earned a Reward','Your referral reward credited to your account. Thanks for using Bridge!!','ENGLISH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'PAYOUT_REWARD','REFERRED_BY_REWARD','💸 Ka-ching! You Just Earned a Reward','Thanks for using Bridge, Keep supporting our drivers !','ENGLISH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'ACCOUNT_DELETED','ACCOUNT_DELETED','Account Deleted!','Your account has been deleted successfully.','ENGLISH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, trip_category, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'ACCOUNT_DELETED','ACCOUNT_DELETED','Delivery_OneWayOnDemandDynamicOffer','Account Deleted!','Your account has been deleted successfully.','ENGLISH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);

INSERT INTO atlas_app.merchant_push_notification (
  fcm_notification_type, key, title, body, language, merchant_id, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'TRIP_FINISHED','TRIP_FINISHED','Trip finished!','Hope you enjoyed your trip with {#driverName#}. Total Fare {#totalFare#}. Please ensure you''ve collected all your belongings before leaving the vehicle!','ENGLISH',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f','f9903ef6-f595-428e-b5ac-e8816cbdf979',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
);


