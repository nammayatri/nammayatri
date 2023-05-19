ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN driver_lat double precision;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN driver_lon double precision;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN driver_dist_to_pickup bigint;