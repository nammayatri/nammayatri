WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'OPERATOR_CONSENT_DEEPLINK_MESSAGE', CAST ('{"var1" : "https://yatrisathi.in/p?vp=wmb_operator_consent"}' AS json),
'Dear User,
Welcome to {#operatorName#} operator! Please confirm your onboarding by clicking this link: {#var1#} -Namma Yatri' -- change merchant name as per requirement
  , 'YTRISI', T1.id
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, json_data, message, sender_header, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'OPERATOR_CONSENT',
    'OPERATOR_CONSENT',
    null,
    moc.merchant_id,
    moc.id,
    'You have joined {#operatorName#} operator!',
    'Welcome to the operator',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'OPERATOR_JOINING_MESSAGE', 'You have been invited to join the operator {#operatorName#}. {#otp#} is your OTP.', T1.id
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);

WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'OPERATOR_JOIN_AND_DOWNLOAD_APP_MESSAGE' AS message_key,
         'You have been invited to join the operator {#fleetOwnerName#}. Download the application to start using it: {# https://nammayatri.in/p/ #}' AS message,
         T1.id AS merchant_operating_city_id
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_UNLINK_FROM_FLEET',
    'DRIVER_UNLINK_FROM_FLEET',
    null,
    moc.merchant_id,
    moc.id,
    'Driver {#driverName#} with phone number {#driverNo#} has been removed from fleet',
    'Driver {#driverName#} with phone number {#driverNo#} has been removed from fleet',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_UNLINK_FROM_OPERATOR',
    'DRIVER_UNLINK_FROM_OPERATOR',
    null,
    moc.merchant_id,
    moc.id,
    'Driver {#driverName#} with phone number {#driverNo#} has been removed from operator',
    'Driver {#driverName#} with phone number {#driverNo#} has been removed from operator',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FLEET_UNLINK_FROM_OPERATOR',
    'FLEET_UNLINK_FROM_OPERATOR',
    null,
    moc.merchant_id,
    moc.id,
    'Fleet {#fleetName#} with phone number {#fleetNo#} has been removed from operator',
    'Fleet {#fleetName#} with phone number {#fleetNo#} has been removed from operator',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;
