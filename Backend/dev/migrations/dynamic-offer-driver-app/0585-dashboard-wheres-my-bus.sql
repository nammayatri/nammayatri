WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'FLEET_CONSENT_DEEPLINK_MESSAGE', CAST ('{"var1" : "https://yatrisathi.in/p?vp=wmb_fleet_consent"}' AS json),
'Dear User,
Welcome to {#fleetOwnerName#}! Please confirm your onboarding by clicking this link: {#var1#} -Namma Yatri' -- change merchant name as per requirement
  , 'YTRISI', T1.id
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, json_data, message, sender_header, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages);

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

INSERT INTO atlas_driver_offer_bpp.fleet_owner_information
  (
    blocked,
    enabled,
    fleet_owner_person_id,
    fleet_type,
    merchant_id,
    verified,
    created_at,
    updated_at
  ) VALUES
  (
    false,
    true,
    'favorit-fleet-owner-0000000000000000',
    'NORMAL_FLEET',
    '94bbea0d-3c52-479b-81f5-eca4969ae797',
    true,
    now(),
    now()
  );

--ONLY_FOR_LOCAL
INSERT INTO atlas_driver_offer_bpp.fleet_owner_information
  (
    blocked,
    enabled,
    fleet_owner_person_id,
    fleet_type,
    merchant_id,
    verified,
    created_at,
    updated_at
  ) VALUES
  (
    false,
    true,
    'favorit-rental_fleet-owner-000000000',
    'RENTAL_FLEET',
    '94bbea0d-3c52-479b-81f5-eca4969ae797',
    true,
    now(),
    now()
  );