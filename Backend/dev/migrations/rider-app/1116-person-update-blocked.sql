ALTER TABLE atlas_app.person ADD COLUMN is_simulated bool;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_simulated_routes text default 'OSRM';
ALTER TABLE atlas_app.person ADD COLUMN action_taken_at timestamp with time zone;
ALTER TABLE atlas_app.person ADD COLUMN action_rule_id text;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_simulated_distance text default 'OSRM';

ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_booking_cancelled_by_driver_count_threshold int;
ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_booking_cancelled_by_driver_count_window json;
ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_booking_cancellation_count_threshold int;
ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_booking_cancellation_count_window json;
ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_booking_total_count_threshold int;
ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_booking_detection_window json;
ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_search_count_threshold int;
ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_search_count_window json;

ALTER TABLE atlas_app.merchant_config DROP COLUMN IF EXISTS fraud_booking_cancelled_by_driver_count_threshold;
ALTER TABLE atlas_app.merchant_config DROP COLUMN IF EXISTS fraud_booking_cancelled_by_driver_count_window;
ALTER TABLE atlas_app.merchant_config DROP COLUMN IF EXISTS fraud_booking_cancellation_count_threshold;
ALTER TABLE atlas_app.merchant_config DROP COLUMN IF EXISTS fraud_booking_cancellation_count_window;
ALTER TABLE atlas_app.merchant_config DROP COLUMN IF EXISTS fraud_booking_total_count_threshold;
ALTER TABLE atlas_app.merchant_config DROP COLUMN IF EXISTS fraud_booking_detection_window;
ALTER TABLE atlas_app.merchant_config DROP COLUMN IF EXISTS fraud_search_count_threshold;
ALTER TABLE atlas_app.merchant_config DROP COLUMN IF EXISTS fraud_search_count_window;