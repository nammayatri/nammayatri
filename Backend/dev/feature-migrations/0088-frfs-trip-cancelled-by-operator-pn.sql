INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type,
    key,
    merchant_id,
    merchant_operating_city_id,
    title,
    body,
    language,
    should_trigger,
    created_at,
    updated_at
)
SELECT
    'CANCELLED_PRODUCT',
    'FRFS_TRIP_CANCELLED_BY_OPERATOR',
    moc.merchant_id,
    moc.id,
    copy.title,
    copy.body,
    copy.language,
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
JOIN (VALUES
    ('Chennai', 'ENGLISH', 'Your bus trip was cancelled', 'The operator cancelled your bus trip. Your money or pass trip will be refunded.'),
    ('Chennai', 'TAMIL', 'உங்கள் பேருந்து பயணம் ரத்து செய்யப்பட்டது', 'ஆபரேட்டர் உங்கள் பேருந்து பயணத்தை ரத்து செய்துள்ளார். உங்கள் பணம் அல்லது பாஸ் பயணம் திருப்பித் தரப்படும்.'),
    ('Kolkata', 'ENGLISH', 'Your bus trip was cancelled', 'The operator cancelled your bus trip. Your money or pass trip will be refunded.'),
    ('Kolkata', 'BENGALI', 'আপনার বাস যাত্রা বাতিল হয়েছে', 'অপারেটর আপনার বাস যাত্রা বাতিল করেছেন। আপনার টাকা বা পাস ট্রিপ ফেরত দেওয়া হবে।')
) AS copy (city, language, title, body) ON copy.city = moc.city
WHERE EXISTS (
    SELECT 1
    FROM atlas_app.integrated_bpp_config ibc
    WHERE ibc.merchant_operating_city_id = moc.id
      AND ibc.vehicle_category = 'BUS'
      AND ibc.platform_type = 'MULTIMODAL'
)
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'FRFS_TRIP_CANCELLED_BY_OPERATOR'
      AND mpn.merchant_operating_city_id = moc.id
      AND mpn.language = copy.language
);
