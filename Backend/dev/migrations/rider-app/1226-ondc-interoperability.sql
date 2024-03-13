-- NOTE (MASTER & PROD):
-- Run migrations of this file only after successful deployment of interoperability changes,
-- as this are breaking changes for older version.
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

ALTER TABLE atlas_app.ride
ALTER COLUMN driver_registered_at DROP NOT NULL;

ALTER TABLE atlas_app.ride
ALTER COLUMN vehicle_color DROP NOT NULL;
