-- ONLY FOR LOCAL --
UPDATE atlas_driver_offer_bpp.person
    SET  merchant_operating_city_id = 'favorit0-0000-0000-0000-00000000city'
    WHERE id IN  ('favorit-auto2-0000000000000000000000', 'favorit-fleet-owner-0000000000000000');

-- ONLY FOR LOCAL --
INSERT INTO atlas_driver_offer_bpp.vehicle_registration_certificate (
    id,
    fleet_owner_id,
    certificate_number_hash,
    certificate_number_encrypted,
    unencrypted_certificate_number,
    fitness_expiry,
    created_at,
    verification_status,
    updated_at,
    failed_rules,
    document_image_id,
    vehicle_variant,
    merchant_id,
    merchant_operating_city_id
) VALUES (
    'auto2-driver-rc-00000000000000000000',
    'favorit-fleet-owner-0000000000000000',
    '\x2f985fd015e80ca2d7482732b6bb5111015eb46000cd1e0d73f66a81a70917a1',
    '0.1.0|1|8rIyA4cI54vwGrwGUFj9izq+74S7jnGrcR9eUE8vOSEWlRQDjT+ecdbjV7vdfymfkvkLpFk14IHexuR+',
    'WB1111111',
    '2099-03-30 00:00:00+00',
    now(),
    'VALID',
    now(),
    '{}',
    'auto2-driver-rc-image-00000000000000',
    'AUTO_RICKSHAW',
    'favorit0-0000-0000-0000-00000favorit',
    'favorit0-0000-0000-0000-00000000city'
    );

-- ONLY FOR LOCAL --
INSERT INTO atlas_driver_offer_bpp.driver_rc_association (
    id,
    driver_id,
    rc_id,
    is_rc_active,
    associated_on,
    associated_till,
    consent,
    consent_timestamp,
    merchant_id,
    merchant_operating_city_id,
    error_message
) VALUES (
    'auto2-driver-rc-association-00000000',
    'favorit-auto2-0000000000000000000000',
    'auto2-driver-rc-00000000000000000000',
    true,
    CURRENT_TIMESTAMP,
    '2099-12-12 00:00:00+00',
    true,
    CURRENT_TIMESTAMP,
    'favorit0-0000-0000-0000-00000favorit',
    'favorit0-0000-0000-0000-00000000city',
    NULL
) ON CONFLICT (id) DO NOTHING;

-- ONLY FOR LOCAL --
INSERT INTO atlas_driver_offer_bpp.driver_license (
    id,
    driver_id,
    merchant_id,
    consent,
    consent_timestamp,
    document_image_id1,
    document_image_id2,
    driver_dob,
    driver_name,
    license_expiry,
    license_number_hash,
    license_number_encrypted,
    verification_status,
    failed_rules,
    class_of_vehicles,
    date_of_issue,
    reject_reason,
    vehicle_category,
    created_at,
    updated_at
) VALUES (
    'favorit-auto2-dl-0000000000000000000',
    'favorit-auto2-0000000000000000000000',
    'favorit0-0000-0000-0000-00000favorit',
    true,
    CURRENT_TIMESTAMP,
    'favorit-auto2-dl-image-0000000000000',
    NULL,
    '1990-01-01 00:00:00+00',
    'Mustafa',
    '2099-12-31 00:00:00+00',
    '\x5ef812c992874fbd16c44ae5b498e8b80f32a5943c76ce645e7e80887c5b29b6',  -- HR12345678901234
    '0.1.0|5|9apKJYcOG4AnBWK7Za9DSOy4pBg4UlSvpBJHdoMNQRo406tcgQh38jxjHKH5FORQffcM/JzTlP9zoATbH9hfeO3xaw==',
    'VALID',
    '{}',
    '{"MCWG", "LMV", "3W-PV"}',
    '2020-01-01 00:00:00+00',
    NULL,
    'CAR',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
);

-- ONLY FOR LOCAL --
INSERT INTO atlas_driver_offer_bpp.reminder_config (
    document_type,
    merchant_id,
    merchant_operating_city_id,
    enabled,
    is_mandatory,
    reminder_intervals,
    days_threshold,
    rides_threshold
) VALUES
  (
    'DriverLicense',
    'favorit0-0000-0000-0000-00000favorit',
    'favorit0-0000-0000-0000-00000000city',
    true,
    true,
    ARRAY[43200, 20160, 10080], -- 30, 14, 7 days in minutes
    30,
    100
  ),
  (
    'VehicleFitnessCertificate',
    'favorit0-0000-0000-0000-00000favorit',
    'favorit0-0000-0000-0000-00000000city',
    true,
    false,
    ARRAY[43200, 20160, 10080], -- 30, 14, 7 days in minutes
    30,
    100
  ),
  (
    'InspectionHub',
    'favorit0-0000-0000-0000-00000favorit',
    'favorit0-0000-0000-0000-00000000city',
    true,
    true,
    ARRAY[0], -- immediate reminder when threshold reached
    NULL,
    3
  ),
  (
    'DriverInspectionHub',
    'favorit0-0000-0000-0000-00000favorit',
    'favorit0-0000-0000-0000-00000000city',
    true,
    true,
    ARRAY[0], -- immediate reminder when threshold reached
    NULL,
    3
  ),
  (
    'TrainingForm',
    'favorit0-0000-0000-0000-00000favorit',
    'favorit0-0000-0000-0000-00000000city',
    true,
    true,
    ARRAY[0], -- immediate reminder when threshold reached
    3,
    NULL
  )
ON CONFLICT (document_type, merchant_operating_city_id) DO NOTHING;

-- ONLY FOR LOCAL --
UPDATE atlas_driver_offer_bpp.transporter_config
  SET reminder_system_enabled = true
  WHERE transporter_config.merchant_operating_city_id = 'favorit0-0000-0000-0000-00000000city';

-- ONLY FOR LOCAL --
-- Merchant messages for reminder notifications
WITH MerchantMessages AS (
  SELECT T1.merchant_id, 'DOCUMENT_EXPIRY_REMINDER_SMS', 'Your {#documentType#} is expiring soon. Please update it to continue driving.', T1.id
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
  UNION ALL
  SELECT T1.merchant_id, 'VEHICLE_INSPECTION_SMS', 'Vehicle inspection is required. Please complete it to continue driving.', T1.id
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
  UNION ALL
  SELECT T1.merchant_id, 'DRIVER_INSPECTION_SMS', 'Driver inspection is required. Please complete it to continue driving.', T1.id
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
  UNION ALL
  SELECT T1.merchant_id, 'TRAINING_VIDEO_SMS', 'Training video completion is required. Please complete it to continue driving.', T1.id
  FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
  (SELECT * FROM MerchantMessages)
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;

-- Merchant push notifications for reminder FCM notifications

-- ONLY FOR LOCAL --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DOCUMENT_EXPIRY_REMINDER',
    'DOCUMENT_EXPIRY_REMINDER',
    null,
    moc.merchant_id,
    moc.id,
    'Document Expiry Reminder',
    'Your {#documentType#} is expiring soon. Please update it to continue driving.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc
ON CONFLICT DO NOTHING;

-- ONLY FOR LOCAL --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'VEHICLE_INSPECTION',
    'VEHICLE_INSPECTION',
    null,
    moc.merchant_id,
    moc.id,
    'Vehicle Inspection Required',
    'Vehicle inspection is required. Please complete it to continue driving.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc
ON CONFLICT DO NOTHING;

-- ONLY FOR LOCAL --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_INSPECTION',
    'DRIVER_INSPECTION',
    null,
    moc.merchant_id,
    moc.id,
    'Driver Inspection Required',
    'Driver inspection is required. Please complete it to continue driving.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc
ON CONFLICT DO NOTHING;

-- ONLY FOR LOCAL --
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRAINING_VIDEO',
    'TRAINING_VIDEO',
    null,
    moc.merchant_id,
    moc.id,
    'Training Video Required',
    'Training video completion is required. Please complete it to continue driving.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc
ON CONFLICT DO NOTHING;
