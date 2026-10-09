-- Fare recompute policy backfill — REQUIRED BEFORE DEPLOYING THE POLICY-ONLY RESOLVER.
--
-- The backend reads ONLY fare_recompute_policy (with code defaults as the
-- fallback; defaults mirror the old column defaults). Any city whose legacy
-- columns carry CUSTOM values must be backfilled BEFORE deploy, or those
-- customizations silently revert to the defaults.
--
-- References only columns that exist in production (the long-standing legacy
-- set). It deliberately does NOT reference no_recompute_trip_categories,
-- actual_ride_duration_diff_threshold or gate_extra_time_charge_by_recompute:
-- those were added and removed within the same unification work and never
-- carried values. If your environment predates the 2026-10-06
-- downward_recompute_distance_threshold column, delete that one line.
--
-- jsonb_strip_nulls drops NULL-valued keys so optional levers stay absent
-- (absent = code default). NOT NULL columns always materialize, pinning each
-- city to exactly the values it bills with today. Behavior-preserving.
-- Legacy DB columns stay in place afterwards (rollback safety) — dead weight.
--
-- Usage: run for all cities, or append
--   AND merchant_operating_city_id = '<city-uuid>'
-- to go city by city (recommended).

UPDATE atlas_driver_offer_bpp.transporter_config
SET fare_recompute_policy = jsonb_strip_nulls(
  jsonb_build_object(
    'upward', jsonb_strip_nulls(jsonb_build_object(
      'allowWithinThreshold', recompute_if_pickup_drop_not_outside_of_threshold,
      -- NULL legacy bands meant upward recompute was DISABLED for the city;
      -- an absent policy key would fall back to the fleet default bands and
      -- silently re-enable it, so NULL must materialize as an explicit [].
      'bands', COALESCE(recompute_distance_thresholds::jsonb, '[]'::jsonb),
      'smallOverageForgivenessMeters', actual_ride_distance_diff_threshold,
      'bufferMeters', upwards_recompute_buffer,
      'bufferPercentage', upwards_recompute_buffer_percentage,
      'dailyExtraKmsBudget', fare_recompute_daily_extra_kms_threshold,
      'weeklyExtraKmsBudget', fare_recompute_weekly_extra_kms_threshold,
      'notifyDriverOnBudgetExceeded', to_notify_driver_for_extra_kms_limit_exceed
    )),
    'downward', jsonb_strip_nulls(jsonb_build_object(
      'allowForChangedDestination', enable_downward_recompute_for_different_destination,
      'forgivenessMeters', downward_recompute_distance_threshold,
      'passThroughMinEstimateMeters', min_threshold_for_pass_through_destination
    )),
    'recomputeCongestionOnEndRide', recompute_congestion_charge_on_end_ride,
    'estimatedTollFallback', enable_estimated_toll_fallback
  )
)::json
WHERE fare_recompute_policy IS NULL;

-- Verify a city after backfill:
--   SELECT merchant_operating_city_id,
--          jsonb_pretty(fare_recompute_policy::jsonb)
--   FROM atlas_driver_offer_bpp.transporter_config
--   WHERE merchant_operating_city_id = '<city-uuid>';
--
-- Sanity checks after a full run:
--   -- every row has a policy
--   SELECT count(*) FROM atlas_driver_offer_bpp.transporter_config WHERE fare_recompute_policy IS NULL;
--   -- spot-check that pinned values match the legacy columns
--   SELECT merchant_operating_city_id,
--          (fare_recompute_policy::jsonb #>> '{upward,bufferMeters}')::numeric AS policy_buffer,
--          upwards_recompute_buffer AS legacy_buffer
--   FROM atlas_driver_offer_bpp.transporter_config
--   WHERE (fare_recompute_policy::jsonb #>> '{upward,bufferMeters}')::numeric
--         IS DISTINCT FROM upwards_recompute_buffer::numeric;
--
-- Remember: transporter_config is cached (Redis + ConfigPilot). After a raw
-- SQL backfill, clear per-city keys
--   driver-offer:CachedQueries:TransporterConfig:MerchantOperatingCityId-<id>
-- and let ConfigPilot's versioned cache expire, or bounce via the dashboard
-- config-update flow which invalidates for you.
