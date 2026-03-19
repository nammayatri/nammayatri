-- Increase max_retry_count for Yatri Sathi (Kolkata) payout config
-- from default 5 to 8, to improve payout success rate for drivers
-- experiencing transient UPI failures.

UPDATE atlas_driver_offer_bpp.payout_config
SET max_retry_count = 8,
    updated_at = now()
WHERE merchant_operating_city_id IN (
    SELECT m.id
    FROM atlas_driver_offer_bpp.merchant_operating_city m
    WHERE m.merchant_short_id = 'JATRI_SAATHI_PARTNER'
      AND m.city = 'Kolkata'
);
