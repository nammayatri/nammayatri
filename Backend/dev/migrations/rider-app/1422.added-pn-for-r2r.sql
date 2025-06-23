INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYOUT_REWARD',
    'REFERRAL_REWARD_ADD_VPA',
    moc.merchant_id,
    moc.id,
    '💸 Ka-ching! You Just Earned a Reward',
    'Your friend''s first ride is complete! Add UPI ID and earn referral reward.',
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
    'REFERRAL_REWARD',
    moc.merchant_id,
    moc.id,
    '💸 Ka-ching! You Just Earned a Reward',
    'Your friend''s first ride is complete! Keep the buzz going—refer more friends and earn for every new rider.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;