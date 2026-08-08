
-- One progress row per driver + journey + milestone + period (Day:/Week:...).
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS idx_incentive_journey_stats_driver_journey_milestone_period
  ON atlas_driver_offer_bpp.incentive_journey_stats USING btree (driver_id, journey_id, milestone_id, period_key);

-- Milestone lookup by journey.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_incentive_journey_milestone_journey_id
  ON atlas_driver_offer_bpp.incentive_journey_milestone USING btree (journey_id);
