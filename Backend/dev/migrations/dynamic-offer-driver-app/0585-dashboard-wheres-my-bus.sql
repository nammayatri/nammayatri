WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'FLEET_CONSENT_DEEPLINK_MESSAGE', CAST ('{"var1" : "https://nammayatri.in/p/?vp=fleet_consent"}' AS json),
'Dear user,
Welcome to {#fleetOwnerName#}!
Please confirm your onboarding by clicking this link: {#var1#}
- Namma Yatri' -- change merchant name as per requirement
  , T1.id
  FROM atlas_app.merchant_operating_city AS T1
);

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FLEET_CONSENT',
    'FLEET_CONSENT',
    null,
    moc.merchant_id,
    moc.id,
    'You have joined the {#fleetOwnerName#} fleet!',
    'Welcome to the fleet',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;