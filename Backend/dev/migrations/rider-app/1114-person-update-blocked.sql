ALTER TABLE atlas_app.person ADD COLUMN is_simulated Bool;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_simulated_routes Text default 'OSRM';
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_simulated_distance Text default 'OSRM';