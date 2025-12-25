
UPDATE atlas_app.merchant_service_usage_config SET get_distances_for_cancel_ride ='OSRM';
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN get_distances_for_cancel_ride SET NOT NULL;
