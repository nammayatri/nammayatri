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
