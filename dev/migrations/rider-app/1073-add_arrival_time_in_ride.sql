ALTER TABLE atlas_app.ride ADD COLUMN driver_arrival_time timestamp with time zone;
ALTER TABLE atlas_app.estimate ADD COLUMN waiting_charge_per_min double precision;
ALTER TABLE atlas_app.estimate ADD COLUMN waiting_time_estimated_threshold int;