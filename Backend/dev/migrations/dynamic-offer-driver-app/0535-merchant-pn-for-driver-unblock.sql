INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'DRIVER_UNBLOCKED',
    'UNBLOCK_DRIVER_KEY',
    moc.merchant_id,
    moc.id,
    'You are now Unblocked',
    'Enjoy your commision free rides!',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;