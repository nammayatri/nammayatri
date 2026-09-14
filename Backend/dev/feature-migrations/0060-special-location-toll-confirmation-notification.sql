
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
    'TOLL_CROSSED',
    'TOLL_CONFIRMATION_REQUIRED',
    src.merchant_id,
    src.merchant_operating_city_id,
    'Confirm toll charges',
    'Your driver has marked a toll on this ride. Share the OTP shown in the app only if the toll was crossed.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_push_notification src
WHERE src.key = 'TOLL_CROSSED'
  AND src.language = 'ENGLISH'
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'TOLL_CONFIRMATION_REQUIRED'
      AND mpn.merchant_operating_city_id = src.merchant_operating_city_id
);
