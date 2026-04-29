------------------------------------------------------------------------------------------------
-- NOTE: This is to fix local schema to make it similar as master schema for config-sync import.
-- Do not run in master or prod
------------------------------------------------------------------------------------------------

-- Align local column types with master to support config-sync import
-- fare_policy: integer -> double precision
ALTER TABLE atlas_driver_offer_bpp.fare_policy ALTER COLUMN per_minute_ride_extra_time_charge TYPE double precision USING per_minute_ride_extra_time_charge::double precision;

-- transporter_config: integer -> bigint
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN driver_auto_pay_execution_time_fall_back TYPE bigint;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN edit_loc_time_threshold TYPE bigint;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN order_and_notification_status_check_fall_back_time TYPE bigint;

-- driver_pool_config: integer -> bigint
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ALTER COLUMN driver_to_destination_distance_threshold TYPE bigint;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ALTER COLUMN driver_to_destination_duration TYPE bigint;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ALTER COLUMN radius_shrink_value_for_drivers_on_ride TYPE bigint;
