-- Enable cancellation penalty system feature flag
ALTER TABLE atlas_driver_offer_bpp.transporter_config
  ADD COLUMN IF NOT EXISTS enable_cancellation_penalties boolean DEFAULT false;

-- Set sensible defaults for cancellation rate blocking thresholds
UPDATE atlas_driver_offer_bpp.transporter_config
  SET cancellation_rate_window = COALESCE(cancellation_rate_window, 7),
      cancellation_rate_calculation_threshold = COALESCE(cancellation_rate_calculation_threshold, 5),
      cancellation_rate_threshold_daily = COALESCE(cancellation_rate_threshold_daily, 25),
      cancellation_rate_threshold_weekly = COALESCE(cancellation_rate_threshold_weekly, 15),
      daily_min_rides_for_blocking = COALESCE(daily_min_rides_for_blocking, 3),
      weekly_min_rides_for_blocking = COALESCE(weekly_min_rides_for_blocking, 10),
      daily_min_rides_for_nudging = COALESCE(daily_min_rides_for_nudging, 2),
      weekly_min_rides_for_nudging = COALESCE(weekly_min_rides_for_nudging, 5),
      daily_offence_suspension_time_hours = COALESCE(daily_offence_suspension_time_hours, 1),
      weekly_offence_suspension_time_hours = COALESCE(weekly_offence_suspension_time_hours, 4),
      daily_condition_cooldown_time_hours = COALESCE(daily_condition_cooldown_time_hours, 6),
      weekly_condition_cooldown_time_hours = COALESCE(weekly_condition_cooldown_time_hours, 24);
