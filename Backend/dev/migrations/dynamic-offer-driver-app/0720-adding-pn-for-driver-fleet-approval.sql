INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'FLEET_REQUEST_APPROVED',
    'FLEET_REQUEST_APPROVED',
    moc.merchant_id,
    moc.id,
    'Fleet Approval Successful',
    'Your request to join fleet has been approved. Start accepting rides now!',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;
