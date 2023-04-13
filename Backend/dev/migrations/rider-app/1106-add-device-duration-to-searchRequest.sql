ALTER TABLE atlas_app.search_request ADD COLUMN device text ;
ALTER TABLE atlas_app.search_request ADD COLUMN estimated_ride_duration integer  ;
ALTER TABLE atlas_app.estimate ADD COLUMN device text ;
ALTER TABLE atlas_app.estimate ADD COLUMN estimated_duration integer  ;
ALTER TABLE atlas_app.estimate ADD COLUMN estimated_distance integer  ;