-- R42: the allocation push says where and when. Existing rows from 0068 (UPDATE, not INSERT). ENGLISH only, like 0068.
-- The rider-app tick always sends vehicleNumber / boardStopCode / etaSeconds (Allocation.notifyDriverOfAllocation).
-- Only rows still carrying 0068's copy are touched, so an edited body is left alone.
UPDATE atlas_driver_offer_bpp.merchant_push_notification
SET body = '{#seats#} seat(s) booked on your shared cab {#vehicleNumber#}. Pick up at {#boardStopCode#} in about {#etaSeconds#}s.',
    updated_at = CURRENT_TIMESTAMP
WHERE key = 'SHARED_CAB_ALLOCATION'
  AND language = 'ENGLISH'
  AND body = '{#seats#} seat(s) booked on your shared cab. Open the route screen for boarding details.';
