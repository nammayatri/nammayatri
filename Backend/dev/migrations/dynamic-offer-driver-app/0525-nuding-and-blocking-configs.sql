-- Do not run in prod or master:
update atlas_driver_offer_bpp.transporter_config set weekly_offence_suspension_time_hours = 2;
-- update atlas_driver_offer_bpp.transporter_config set weekly_min_rides_for_nudging = 0;
-- update atlas_driver_offer_bpp.transporter_config set weekly_min_rides_for_blocking = 0;
-- update atlas_driver_offer_bpp.transporter_config set daily_offence_suspension_time_hours = 1;
-- update atlas_driver_offer_bpp.transporter_config set daily_min_rides_for_nudging = 0;
-- update atlas_driver_offer_bpp.transporter_config set daily_min_rides_for_blocking = 0;
-- update atlas_driver_offer_bpp.transporter_config set cancellation_rate_threshold_weekly = 50;
-- update atlas_driver_offer_bpp.transporter_config set cancellation_rate_threshold_daily = 50;

-- Do not run in prod or master:
-- update atlas_driver_offer_bpp.transporter_config set cancellation_rate_window = 15;
-- update atlas_driver_offer_bpp.transporter_config set cancellation_rate_calculation_threshold = 0;

-- insert into atlas_driver_offer_bpp.namma_tag(category, tag_type, name, created_at, updated_at, rule_engine, event, tags) values('DriverCancellation
-- Validity','ApplicationTag','DriverCancellation',CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, '[{"if": ["Valid", "Valid"]}]','RideCancel','{Valid,Invalid}');