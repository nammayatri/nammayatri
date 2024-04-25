CREATE INDEX CONCURRENTLY idx_ride_driver_mobile_number_and_status ON atlas_app.ride USING btree (driver_mobile_number, status);
