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
    copy.key,
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
    ('Chennai', 'ENGLISH', 'FRFS_TRIP_CANCELLED_BY_OPERATOR_ONLINE', 'Your bus trip was cancelled', 'The operator cancelled your bus trip. Your money will be refunded.'),
    ('Chennai', 'ENGLISH', 'FRFS_TRIP_CANCELLED_BY_OPERATOR_PASS', 'Your bus trip was cancelled', 'The operator cancelled your bus trip. Your pass trip has been restored.'),
    ('Chennai', 'ENGLISH', 'FRFS_TRIP_CANCELLED_BY_OPERATOR_PASS_PARTIAL', 'Your bus trip was cancelled', 'The operator cancelled your bus trip. Your pass trip has been restored and your money will be refunded.'),
    ('Chennai', 'TAMIL', 'FRFS_TRIP_CANCELLED_BY_OPERATOR_ONLINE', 'உங்கள் பேருந்து பயணம் ரத்து செய்யப்பட்டது', 'ஆபரேட்டர் உங்கள் பேருந்து பயணத்தை ரத்து செய்துள்ளார். உங்கள் பணம் திருப்பித் தரப்படும்.'),
    ('Chennai', 'TAMIL', 'FRFS_TRIP_CANCELLED_BY_OPERATOR_PASS', 'உங்கள் பேருந்து பயணம் ரத்து செய்யப்பட்டது', 'ஆபரேட்டர் உங்கள் பேருந்து பயணத்தை ரத்து செய்துள்ளார். உங்கள் பாஸ் பயணம் மீண்டும் சேர்க்கப்பட்டது.'),
    ('Chennai', 'TAMIL', 'FRFS_TRIP_CANCELLED_BY_OPERATOR_PASS_PARTIAL', 'உங்கள் பேருந்து பயணம் ரத்து செய்யப்பட்டது', 'ஆபரேட்டர் உங்கள் பேருந்து பயணத்தை ரத்து செய்துள்ளார். உங்கள் பாஸ் பயணம் மீண்டும் சேர்க்கப்பட்டது, உங்கள் பணம் திருப்பித் தரப்படும்.'),
    ('Kolkata', 'ENGLISH', 'FRFS_TRIP_CANCELLED_BY_OPERATOR_ONLINE', 'Your bus trip was cancelled', 'The operator cancelled your bus trip. Your money will be refunded.'),
    ('Kolkata', 'ENGLISH', 'FRFS_TRIP_CANCELLED_BY_OPERATOR_PASS', 'Your bus trip was cancelled', 'The operator cancelled your bus trip. Your pass trip has been restored.'),
    ('Kolkata', 'ENGLISH', 'FRFS_TRIP_CANCELLED_BY_OPERATOR_PASS_PARTIAL', 'Your bus trip was cancelled', 'The operator cancelled your bus trip. Your pass trip has been restored and your money will be refunded.'),
    ('Kolkata', 'BENGALI', 'FRFS_TRIP_CANCELLED_BY_OPERATOR_ONLINE', 'আপনার বাস যাত্রা বাতিল হয়েছে', 'অপারেটর আপনার বাস যাত্রা বাতিল করেছেন। আপনার টাকা ফেরত দেওয়া হবে।'),
    ('Kolkata', 'BENGALI', 'FRFS_TRIP_CANCELLED_BY_OPERATOR_PASS', 'আপনার বাস যাত্রা বাতিল হয়েছে', 'অপারেটর আপনার বাস যাত্রা বাতিল করেছেন। আপনার পাস ট্রিপ ফেরত দেওয়া হয়েছে।'),
    ('Kolkata', 'BENGALI', 'FRFS_TRIP_CANCELLED_BY_OPERATOR_PASS_PARTIAL', 'আপনার বাস যাত্রা বাতিল হয়েছে', 'অপারেটর আপনার বাস যাত্রা বাতিল করেছেন। আপনার পাস ট্রিপ ফেরত দেওয়া হয়েছে এবং আপনার টাকা ফেরত দেওয়া হবে।')
) AS copy (city, language, key, title, body) ON copy.city = moc.city
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
    WHERE mpn.key = copy.key
      AND mpn.merchant_operating_city_id = moc.id
      AND mpn.language = copy.language
);

INSERT INTO atlas_app.merchant_message (
    merchant_id,
    merchant_operating_city_id,
    message_key,
    message,
    template_id,
    contains_url_button,
    created_at,
    updated_at
)
SELECT
    moc.merchant_id,
    moc.id,
    msg.message_key,
    msg.message,
    '',
    false,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
CROSS JOIN (VALUES
    ('WHATSAPP_FRFS_TRIP_CANCELLED_BY_OPERATOR_ONLINE', E'Your bus trip from {#fromStop#} to {#toStop#} was cancelled by the operator.\n\nYour money will be refunded.'),
    ('WHATSAPP_FRFS_TRIP_CANCELLED_BY_OPERATOR_PASS', E'Your bus trip from {#fromStop#} to {#toStop#} was cancelled by the operator.\n\nYour pass trip has been restored.'),
    ('WHATSAPP_FRFS_TRIP_CANCELLED_BY_OPERATOR_PASS_PARTIAL', E'Your bus trip from {#fromStop#} to {#toStop#} was cancelled by the operator.\n\nYour pass trip has been restored and your money will be refunded.')
) AS msg (message_key, message)
WHERE moc.city IN ('Chennai', 'Kolkata')
  AND EXISTS (
    SELECT 1
    FROM atlas_app.integrated_bpp_config ibc
    WHERE ibc.merchant_operating_city_id = moc.id
      AND ibc.vehicle_category = 'BUS'
      AND ibc.platform_type = 'MULTIMODAL'
)
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;
