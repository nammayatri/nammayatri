INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYOUT_REWARD',
    'REFERRED_BY_REWARD_ADD_VPA',
    moc.merchant_id,
    moc.id,
    '💸 Ka-ching! You Just Earned a Reward',
    'Add UPI ID and earn the reward. Thanks for using Namma Yatri, Keep supporting our drivers !!',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYOUT_REWARD',
    'REFERRED_BY_REWARD',
    moc.merchant_id,
    moc.id,
    '💸 Ka-ching! You Just Earned a Reward',
    'Thanks for using Namma Yatri, Keep supporting our drivers !!',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;