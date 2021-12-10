UPDATE atlas_app.ride_booking AS T1 SET bpp_ride_booking_id = 'UNKNOWN';
ALTER TABLE atlas_app.ride_booking ALTER COLUMN bpp_ride_booking_id SET NOT NULL;