ALTER TABLE atlas_transporter.ride ADD COLUMN trip_start_lat double precision;
ALTER TABLE atlas_transporter.ride ADD COLUMN trip_start_lon double precision;
ALTER TABLE atlas_transporter.ride ADD COLUMN trip_end_lat double precision;
ALTER TABLE atlas_transporter.ride ADD COLUMN trip_end_lon double precision;

ALTER TABLE atlas_transporter.transporter_config ALTER COLUMN key TYPE varchar(255) USING key::varchar;
UPDATE atlas_transporter.transporter_config SET key='radius' WHERE key='ConfigKey {getConfigKey = "radius"}';
