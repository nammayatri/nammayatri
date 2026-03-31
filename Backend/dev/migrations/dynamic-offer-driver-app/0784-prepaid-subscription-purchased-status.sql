-- Normalize queued prepaid subscription purchases to PURCHASED so only one ACTIVE remains.
UPDATE atlas_driver_offer_bpp.subscription_purchase
SET status = 'PURCHASED',
    updated_at = CURRENT_TIMESTAMP
WHERE service_name = 'PREPAID_SUBSCRIPTION'
  AND status = 'ACTIVE'
  AND expiry_date IS NULL
  AND start_date IS NULL;
