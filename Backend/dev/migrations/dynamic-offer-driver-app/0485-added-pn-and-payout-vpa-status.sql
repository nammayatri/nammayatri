UPDATE atlas_driver_offer_bpp.driver_information SET payout_vpa_status = 'VIA_WEBHOOK' where payout_vpa is not null;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYOUT_VPA_REMINDER',
    'PAYOUT_VPA_ALERT',
    moc.merchant_id,
    moc.id,
    'Add UPI ID to receive referral bonus of ₹{#rewardAmount#}',
    'Referral bonus transaction pending due to missing UPI ID',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'PAYOUT_REWARD',
    'PAYOUT_REFERRAL_REWARD',
    moc.merchant_id,
    moc.id,
    '₹{#rewardAmount#} Referral Bonus Earned',
    'Referred customer completed their first ride. Keep referring and earn more.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;