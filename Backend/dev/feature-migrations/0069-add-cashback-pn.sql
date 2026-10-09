-- Cashback push notifications fired on ride start and ride end, only when the
-- ride earned a cashback offer. Two keys, picked in code by whether the rider
-- has added a payout VPA (UPI):
--   CASHBACK_ON_ITS_WAY   -> rider has a UPI/VPA ("cashback on its way")
--   ADD_UPI_FOR_CASHBACK  -> rider has no UPI/VPA ("add your UPI to receive it")
-- Placeholder: cashbackAmount
-- fcm_notification_type = 'TRIGGER_FCM' (generic FCM category, same as other
-- data-driven PNs like REWARD_UNLOCK). trip_category / fcm_sub_category left NULL
-- so the lookup falls back to these for any trip category, and language falls
-- back to ENGLISH.

-- Rider HAS added UPI/VPA
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id,
    title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'TRIGGER_FCM',
    'CASHBACK_ON_ITS_WAY',
    moc.merchant_id,
    moc.id,
    'Cashback on its way! 🎉',
    'Your {#cashbackAmount#} cashback for this ride is on its way to your UPI.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE NOT EXISTS (
    SELECT 1 FROM atlas_app.merchant_push_notification pn
    WHERE pn.merchant_operating_city_id = moc.id
      AND pn.key = 'CASHBACK_ON_ITS_WAY'
      AND pn.language = 'ENGLISH'
);

-- Rider has NOT added UPI/VPA
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id,
    title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'TRIGGER_FCM',
    'ADD_UPI_FOR_CASHBACK',
    moc.merchant_id,
    moc.id,
    'Add your UPI to get cashback 💸',
    'You''ve earned {#cashbackAmount#} cashback on this ride! Add your UPI ID in the app to receive it.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE NOT EXISTS (
    SELECT 1 FROM atlas_app.merchant_push_notification pn
    WHERE pn.merchant_operating_city_id = moc.id
      AND pn.key = 'ADD_UPI_FOR_CASHBACK'
      AND pn.language = 'ENGLISH'
);
