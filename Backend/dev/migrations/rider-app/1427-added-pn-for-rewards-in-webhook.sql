INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYOUT_REWARD',
    'REFERRAL_BONUS_EARNED',
    moc.merchant_id,
    moc.id,
    '💸 Ka-ching! You Just Earned a Reward',
    'Your referral reward credited to your account. Thanks for using Namma Yatri!!',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;
