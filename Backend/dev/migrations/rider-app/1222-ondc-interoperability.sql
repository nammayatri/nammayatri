ALTER TABLE atlas_app.driver_offer
ALTER COLUMN distance_to_pickup DROP NOT NULL;

ALTER TABLE atlas_app.driver_offer
ALTER COLUMN duration_to_pickup DROP NOT NULL;

ALTER TABLE atlas_app.quote
ALTER COLUMN provider_mobile_number DROP NOT NULL;

ALTER TABLE atlas_app.quote
ALTER COLUMN provider_name DROP NOT NULL;

ALTER TABLE atlas_app.quote
ALTER COLUMN provider_completed_rides_count DROP NOT NULL;

ALTER TABLE atlas_app.booking
ALTER COLUMN provider_mobile_number DROP NOT NULL;

ALTER TABLE atlas_app.booking
ALTER COLUMN provider_name DROP NOT NULL;
