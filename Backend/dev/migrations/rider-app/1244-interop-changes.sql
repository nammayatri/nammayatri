-- ALTER TABLE atlas_app.booking
-- ADD COLUMN payment_status text NULL;

ALTER TABLE atlas_app.ride
ALTER COLUMN driver_registered_at DROP NOT NULL;

ALTER TABLE atlas_app.ride
ALTER COLUMN vehicle_color DROP NOT NULL;
