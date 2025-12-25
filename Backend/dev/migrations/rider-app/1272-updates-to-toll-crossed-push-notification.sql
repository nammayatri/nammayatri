--ONLY FOR MASTER
DELETE FROM atlas_app.merchant_push_notification WHERE key = 'TOLL_CROSSED' AND merchant_operating_city_id = 'b30daaf7-77d2-17c8-00d9-baf7ad0f5719';
-----------------

-- QUERY FOR PROD
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TOLL_CROSSED',
    'TOLL_CROSSED',
    moc.merchant_id,
    moc.id,
    'Toll Nearby !!',
    'Charges will be added to final fare if toll crossed during the ride',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
WHERE
    moc.id != (SELECT DISTINCT merchant_operating_city_id
               FROM atlas_app.merchant_push_notification
               WHERE key = 'TOLL_CROSSED');