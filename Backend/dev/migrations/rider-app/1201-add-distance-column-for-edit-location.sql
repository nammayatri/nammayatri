ALTER TABLE atlas_app.merchant ADD COLUMN edit_pickup_distance_threshold double precision DEFAULT 100 NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN allowed_edit_location_attempts int;
ALTER TABLE atlas_app.merchant ADD COLUMN driver_distance_threshold_from_pickup double precision DEFAULT 100 NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN num_of_allowed_edit_pickup_location_attempts_threshold int DEFAULT 2 NOT NULL;
