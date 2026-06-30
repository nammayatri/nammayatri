-- Ride-offer cashback payout status push notifications.
--   key (merchant_push_notification lookup): OFFER_CASHBACK_INITIATED / OFFER_CASHBACK_FAILED / OFFER_CASHBACK_COMPLETED
--   fcm_notification_type (FCM Category): PAYOUT_INITIATED / PAYOUT_FAILED / PAYOUT_COMPLETED
-- Placeholder: amount (substituted at send time via {#amount#}).

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'PAYOUT_INITIATED',
    'OFFER_CASHBACK_INITIATED',
    moc.merchant_id,
    moc.id,
    'Cashback payout initiated',
    'Your offer cashback payout of ₹{#amount#} has been initiated.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'PAYOUT_FAILED',
    'OFFER_CASHBACK_FAILED',
    moc.merchant_id,
    moc.id,
    'Cashback payout failed',
    'Your offer cashback payout of ₹{#amount#} could not be processed.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'PAYOUT_COMPLETED',
    'OFFER_CASHBACK_COMPLETED',
    moc.merchant_id,
    moc.id,
    'Cashback credited',
    'Your offer cashback of ₹{#amount#} has been credited.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;
