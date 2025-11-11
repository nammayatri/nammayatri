INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type,
    key,
    merchant_id,
    merchant_operating_city_id,
    title,
    body,
    language,
    created_at,
    updated_at
)
SELECT
    data.fcm_notification_type,
    data.key,
    moc.merchant_id,
    moc.id,
    data.title,
    data.body,
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
    CROSS JOIN (
        VALUES
            ('REFUND_PENDING', 'Normal_REFUND_PENDING', 'Refund Initiated', 'Your refund is being processed. It is expected to be completed within 14 working days.'),
            ('REFUND_SUCCESS', 'Normal_REFUND_SUCCESS', 'Refund Processed ', 'Your refund has been processed successfully. Please check your bank account for details.'),
            ('REFUND_FAILED', 'Normal_REFUND_FAILED', 'Refund Failed ', 'Your refund wasn''t processed. Tap to retry.'),
            ('REFUND_PENDING', 'FRFSBooking_REFUND_PENDING', 'Refund Initiated', 'Your refund is being processed. It is expected to be completed within 14 working days.'),
            ('REFUND_SUCCESS', 'FRFSBooking_REFUND_SUCCESS', 'Refund Processed ', 'Your refund has been processed successfully. Please check your bank account for details.'),
            ('REFUND_FAILED', 'FRFSBooking_REFUND_FAILED', 'Refund Failed ', 'Your refund wasn''t processed. Tap to retry.'),
            ('REFUND_PENDING', 'FRFSBusBooking_REFUND_PENDING', 'Refund Initiated', 'Your refund is being processed. It is expected to be completed within 14 working days.'),
            ('REFUND_SUCCESS', 'FRFSBusBooking_REFUND_SUCCESS', 'Refund Processed ', 'Your refund has been processed successfully. Please check your bank account for details.'),
            ('REFUND_FAILED', 'FRFSBusBooking_REFUND_FAILED', 'Refund Failed ', 'Your refund wasn''t processed. Tap to retry.'),
            ('REFUND_PENDING', 'FRFSMultiModalBooking_REFUND_PENDING', 'Refund Initiated', 'Your refund is being processed. It is expected to be completed within 14 working days.'),
            ('REFUND_SUCCESS', 'FRFSMultiModalBooking_REFUND_SUCCESS', 'Refund Processed ', 'Your refund has been processed successfully. Please check your bank account for details.'),
            ('REFUND_FAILED', 'FRFSMultiModalBooking_REFUND_FAILED', 'Refund Failed ', 'Your refund wasn''t processed. Tap to retry.'),
            ('REFUND_PENDING', 'FRFSPassPurchase_REFUND_PENDING', 'Refund Initiated', 'Your refund is being processed. It is expected to be completed within 14 working days.'),
            ('REFUND_SUCCESS', 'FRFSPassPurchase_REFUND_SUCCESS', 'Refund Processed ', 'Your refund has been processed successfully. Please check your bank account for details.'),
            ('REFUND_FAILED', 'FRFSPassPurchase_REFUND_FAILED', 'Refund Failed ', 'Your refund wasn''t processed. Tap to retry.'),
            ('REFUND_PENDING', 'BBPS_REFUND_PENDING', 'Refund Initiated', 'Your refund is being processed. It is expected to be completed within 14 working days.'),
            ('REFUND_SUCCESS', 'BBPS_REFUND_SUCCESS', 'Refund Processed ', 'Your refund has been processed successfully. Please check your bank account for details.'),
            ('REFUND_FAILED', 'BBPS_REFUND_FAILED', 'Refund Failed ', 'Your refund wasn''t processed. Tap to retry.')
    ) AS data (fcm_notification_type, key, title, body);

ALTER TABLE atlas_app.payment_order ADD COLUMN payment_fulfillment_status TEXT;