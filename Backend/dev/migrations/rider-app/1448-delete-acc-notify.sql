-- Run this query in Prod, don't run in Master
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'ACCOUNT_DELETED',
    'ACCOUNT_DELETED',
    'Delivery_OneWayOnDemandDynamicOffer',
    moc.merchant_id,
    moc.id,
    'Account Deleted!',
    'Your account has been deleted successfully.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;
