-- please run this query in master & prod release

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FCM_CHAT_MESSAGE',
    'FCM_CHAT_MESSAGE',
    moc.merchant_id,
    moc.id,
    'Missed Call: Quick Action Required',
    'Your driver tried to contact you but couldn''t get through. Please call them back to coordinate your pickup.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;
