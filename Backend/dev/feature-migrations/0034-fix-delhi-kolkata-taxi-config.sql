-- Fix: One Delhi MOC (2a95be1d) has search_repeat_limit = 0.
-- checkIfRepeatSearch in SharedLogic/Cancel.hs gates reallocation on:
--   searchTry.searchRepeatCounter < searchRepeatLimit
-- With limit = 0, this is always 0 < 0 = False, so D1 driver cancel never
-- triggers reallocation to D2. Same root cause as Chennai (fixed in 0033).
-- Set the affected Delhi MOC to 3 to allow D2 to receive the ride.
UPDATE atlas_driver_offer_bpp.transporter_config
SET search_repeat_limit = 3
WHERE merchant_operating_city_id = '2a95be1d-9052-4715-8cf5-ea8f68ffc85a'
AND search_repeat_limit = 0;

-- Fix: Delhi and Kolkata driver_pool_config has single_batch_process_time of 22-25s.
-- SRFD searchRequestValidTill = createdAt + singleBatchProcessTime. The allocator
-- takes ~17s to create SRFD after search, and the test polls ~25s after creation
-- → SRFD already expired (empty searchRequestsForDriver). Bangalore/Chennai work at 60s;
-- set Delhi and Kolkata to 60s to match.
UPDATE atlas_driver_offer_bpp.driver_pool_config
SET single_batch_process_time = 60
WHERE merchant_operating_city_id IN (
    SELECT moc.id FROM atlas_driver_offer_bpp.merchant_operating_city moc
    WHERE moc.city IN ('Delhi', 'Kolkata')
)
AND single_batch_process_time < 60;
