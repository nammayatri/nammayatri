ALTER TABLE atlas_app.booking ADD COLUMN is_scheduled boolean;
ALTER TABLE atlas_app.merchant ADD COLUMN schedule_ride_buffer_time integer default 1800;