INSERT INTO atlas_driver_offer_bpp.reminder_config (
    document_type,
    merchant_id,
    merchant_operating_city_id,
    enabled,
    is_mandatory,
    reminder_intervals,
    days_threshold,
    rides_threshold,
    reminder_reschedule_interval_seconds,
    reminder_on_ride_reschedule_interval_seconds
)
SELECT
    d.document_type,
    m.merchant_id,
    m.id,
    d.enabled,
    d.is_mandatory,
    d.reminder_intervals,
    d.days_threshold,
    d.rides_threshold,
    d.reminder_reschedule_interval_seconds,
    d.reminder_on_ride_reschedule_interval_seconds
FROM atlas_driver_offer_bpp.merchant_operating_city as m
CROSS JOIN (
    VALUES
        ('DriverLicense', true, true, ARRAY[15, 10, 5], null, null, 86400, 7200),
        ('VehicleRegistrationCertificate', true, true, ARRAY[15, 10, 5], null, null, 86400, 7200),
        ('BusinessLicense', true, true, ARRAY[15, 10, 5], null, null, 86400, 7200),
        ('VehicleInsurance', true, true, ARRAY[15, 10, 5], null, null, 86400, 7200),
        ('VehiclePUC', true, false, ARRAY[15, 10, 5], null, null, 86400, 7200),
        ('VehiclePermit', true, false, ARRAY[15, 10, 5], null, null, 86400, 7200),
        ('VehicleFitnessCertificate', false, false, ARRAY[15, 10, 5], null, null, 86400, 7200),
        ('VehicleInspectionForm', true, true, ARRAY[15, 10, 5], null, 3, 86400, 7200),
        ('DriverInspectionForm', true, true, ARRAY[15, 10, 5], null, 3, 86400, 7200),
        ('TrainingForm', true, false, ARRAY[15, 10, 5], 1, null, 86400, 7200)
) as d(document_type, enabled, is_mandatory, reminder_intervals, days_threshold, rides_threshold, reminder_reschedule_interval_seconds, reminder_on_ride_reschedule_interval_seconds)
WHERE m.merchant_short_id = 'MSIL_PARTNER'
AND m.city = 'Delhi';

-- Merchant messages for reminder notifications

INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
SELECT
    m.merchant_id,
    'DOCUMENT_EXPIRY_REMINDER_SMS',
    'Your {#documentType#} is expiring soon. Please update it to continue driving.',
    m.id
FROM atlas_driver_offer_bpp.merchant_operating_city AS m
WHERE m.merchant_short_id = 'MSIL_PARTNER'
AND m.city = 'Delhi'
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
SELECT
    m.merchant_id,
    'VEHICLE_INSPECTION_SMS',
    'Vehicle inspection is required. Please complete it to continue driving.',
    m.id
FROM atlas_driver_offer_bpp.merchant_operating_city AS m
WHERE m.merchant_short_id = 'MSIL_PARTNER'
AND m.city = 'Delhi'
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
SELECT
    m.merchant_id,
    'DRIVER_INSPECTION_SMS',
    'Driver inspection is required. Please complete it to continue driving.',
    m.id
FROM atlas_driver_offer_bpp.merchant_operating_city AS m
WHERE m.merchant_short_id = 'MSIL_PARTNER'
AND m.city = 'Delhi'
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
SELECT
    m.merchant_id,
    'TRAINING_VIDEO_SMS',
    'Training video completion is required. Please complete it to continue driving.',
    m.id
FROM atlas_driver_offer_bpp.merchant_operating_city AS m
WHERE m.merchant_short_id = 'MSIL_PARTNER'
AND m.city = 'Delhi'
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;

-- Merchant push notifications for reminder FCM notifications

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    id, fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    atlas_driver_offer_bpp.uuid_generate_v4(),
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
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'MSIL_PARTNER'
AND moc.city = 'Delhi'
ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    id, fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    atlas_driver_offer_bpp.uuid_generate_v4(),
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
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'MSIL_PARTNER'
AND moc.city = 'Delhi'
ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    id, fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    atlas_driver_offer_bpp.uuid_generate_v4(),
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
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'MSIL_PARTNER'
AND moc.city = 'Delhi'
ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    id, fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    atlas_driver_offer_bpp.uuid_generate_v4(),
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
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'MSIL_PARTNER'
AND moc.city = 'Delhi'
ON CONFLICT DO NOTHING;
