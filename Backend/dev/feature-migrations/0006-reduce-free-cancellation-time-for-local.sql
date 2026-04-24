-- Reduce free_cancellation_time_seconds to 30s in local dev so integration tests
-- (OnlineRideBookingFlow/02-StripeRideUserCancellation) can cross the no-show
-- window quickly instead of busy-waiting the prod default of 120s.
UPDATE atlas_driver_offer_bpp.cancellation_fare_policy
SET free_cancellation_time_seconds = 30;
