-- Per-event provider routing for event tracking.
--
-- Maps an event name to the subset of providers that should receive it, e.g.
--   {"ny_user_first_ride_completed": ["Moengage"]}
--
-- An event absent from this map is sent to every provider listed in
-- event_tracking_providers. NULL therefore preserves existing behaviour, so no
-- backfill is required.
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN IF NOT EXISTS event_tracking_overrides jsonb;
