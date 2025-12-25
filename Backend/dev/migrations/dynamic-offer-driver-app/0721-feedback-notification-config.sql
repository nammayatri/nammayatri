-- QUERIES FOR MASTER
UPDATE atlas_driver_offer_bpp.transporter_config
SET feedback_notification_config = '{"enableFeedbackNotification":true,"allowNotificationOnEmptyBadge":true,"feedbackNotificationDelayInSec":60}'
WHERE feedback_notification_config IS NULL;

-- QUERIES FOR PROD
-- UPDATE atlas_driver_offer_bpp.transporter_config
-- SET feedback_notification_config = '{"enableFeedbackNotification":false,"enableFeedbackNotification":false,"feedbackNotificationDelayInSec":7200}'
-- WHERE feedback_notification_config IS NULL;

-- Insert FEEDBACK_BADGE_PN notification for ENGLISH
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id,
    title, body, language, created_at, updated_at
)
SELECT
    'FEEDBACK_BADGE_PN',
    'FEEDBACK_BADGE_PN',
    null,
    moc.merchant_id,
    moc.id,
    'Feedback received',
    'Feedback received',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

-- Insert FEEDBACK_BADGE_PN notification for HINDI
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id,
    title, body, language, created_at, updated_at
)
SELECT
    'FEEDBACK_BADGE_PN',
    'FEEDBACK_BADGE_PN',
    null,
    moc.merchant_id,
    moc.id,
    'प्रतिक्रिया प्राप्त हुई',
    'प्रतिक्रिया प्राप्त हुई',
    'HINDI',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

-- Insert FEEDBACK_BADGE_PN notification for KANNADA
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id,
    title, body, language, created_at, updated_at
)
SELECT
    'FEEDBACK_BADGE_PN',
    'FEEDBACK_BADGE_PN',
    null,
    moc.merchant_id,
    moc.id,
    'ಪ್ರತಿಕ್ರಿಯೆ ಸಿಕ್ಕಿತು',
    'ಪ್ರತಿಕ್ರಿಯೆ ಸಿಕ್ಕಿತು',
    'KANNADA',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

-- Insert FEEDBACK_BADGE_PN notification for TAMIL
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id,
    title, body, language, created_at, updated_at
)
SELECT
    'FEEDBACK_BADGE_PN',
    'FEEDBACK_BADGE_PN',
    null,
    moc.merchant_id,
    moc.id,
    'கருத்து கிடைத்தது',
    'கருத்து கிடைத்தது',
    'TAMIL',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

-- Insert FEEDBACK_BADGE_PN notification for TELUGU
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id,
    title, body, language, created_at, updated_at
)
SELECT
    'FEEDBACK_BADGE_PN',
    'FEEDBACK_BADGE_PN',
    null,
    moc.merchant_id,
    moc.id,
    'అభిప్రాయం అందింది',
    'అభిప్రాయం అందింది',
    'TELUGU',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

-- Insert FEEDBACK_BADGE_PN notification for BENGALI
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id,
    title, body, language, created_at, updated_at
)
SELECT
    'FEEDBACK_BADGE_PN',
    'FEEDBACK_BADGE_PN',
    null,
    moc.merchant_id,
    moc.id,
    'মতামত পাওয়া গেছে',
    'মতামত পাওয়া গেছে',
    'BENGALI',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

-- Insert FEEDBACK_BADGE_PN notification for ODIA
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id,
    title, body, language, created_at, updated_at
)
SELECT
    'FEEDBACK_BADGE_PN',
    'FEEDBACK_BADGE_PN',
    null,
    moc.merchant_id,
    moc.id,
    'ମତାମତ ମିଳିଲା',
    'ମତାମତ ମିଳିଲା',
    'ODIA',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;
