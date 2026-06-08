-- Issue: Chennai (NAMMA_YATRI_PARTNER) has search_repeat_limit = 0 in production config.
-- The reallocation condition is (searchRepeatCounter < searchRepeatLimit), so with limit=0
-- the check (0 < 0) is always false — reallocation never triggers for Chennai.
-- This causes the "NY Auto Driver Cancellation + Reallocation" suite in RideBookingFlow
-- to fail at "D2 Get Nearby Ride Requests" with an empty response.
-- Fix: Set search_repeat_limit = 3 for Chennai locally (same as Bangalore) so that
-- driver cancellation correctly triggers reallocation and D2 receives the search request.
UPDATE atlas_driver_offer_bpp.transporter_config
SET search_repeat_limit = 3
WHERE merchant_operating_city_id = 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da';
