DELETE FROM atlas_driver_offer_bpp.merchant_push_notification WHERE key = 'TOLL_CROSSED' AND merchant_operating_city_id = '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98';

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
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
    atlas_driver_offer_bpp.merchant_operating_city moc
WHERE
    moc.id != (SELECT DISTINCT merchant_operating_city_id
               FROM atlas_driver_offer_bpp.merchant_push_notification
               WHERE key = 'TOLL_CROSSED');