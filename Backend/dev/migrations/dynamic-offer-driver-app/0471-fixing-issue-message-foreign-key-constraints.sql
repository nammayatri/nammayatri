ALTER TABLE atlas_driver_offer_bpp.issue_message DROP CONSTRAINT issue_message_reference_category_id_fkey;

ALTER TABLE atlas_driver_offer_bpp.issue_message
ADD CONSTRAINT issue_message_reference_category_id_fkey
FOREIGN KEY (reference_category_id)
REFERENCES atlas_driver_offer_bpp.issue_category(id);

INSERT INTO atlas_driver_offer_bpp.payout_config(
    vehicle_category,
    updated_at,time_diff,
    threshold_payout_amount_per_person,
    referral_reward_amount_per_ride,
    payout_registration_sgst,
    payout_registration_fee,
    payout_registration_cgst,
    merchant_operating_city_id,
    merchant_id,
    max_retry_count,
    is_payout_enabled,
    created_at,
    batch_limit,
    remark,
    order_type
    )
SELECT
    vehicle_category_type,
    CURRENT_TIMESTAMP,
    86400,
    1000.0,
    100.0,
    0.0,
    2.0,
    0.0,
    city.id,
    city.merchant_id,
    0,
    FALSE,
    CURRENT_TIMESTAMP,
    10,
    'Referral Reward From Nammayatri',
    'FULFILL_ONLY'
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS city
CROSS JOIN
    (VALUES
        ('CAR'),
        ('MOTORCYCLE'),
        ('TRAIN'),
        ('BUS'),
        ('FLIGHT'),
        ('AUTO_CATEGORY'),
        ('AMBULANCE')
    ) AS vehicle_category (vehicle_category_type);