-- Last completed ride of the rider, set with total_rides_count at ride end and read by offer eligibility rules.
ALTER TABLE atlas_app.person ADD COLUMN IF NOT EXISTS last_ride_end_time timestamp with time zone;
ALTER TABLE atlas_app.person ADD COLUMN IF NOT EXISTS last_ride_rating integer;
