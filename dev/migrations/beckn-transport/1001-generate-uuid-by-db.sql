CREATE OR REPLACE FUNCTION atlas_transporter.uuid_generate_v4() RETURNS character (36)
    AS 'SELECT uuid_in(overlay(overlay(md5(random()::text || '':'' || clock_timestamp()::text) placing ''4'' from 13) placing to_hex(floor(random()*(11-8+1) + 8)::int)::text from 17)::cstring)::character (36);'
    LANGUAGE SQL
    IMMUTABLE;

ALTER TABLE atlas_transporter.search_request ALTER COLUMN id SET DEFAULT atlas_transporter.uuid_generate_v4();
ALTER TABLE atlas_transporter.quote ALTER COLUMN id SET DEFAULT atlas_transporter.uuid_generate_v4();
ALTER TABLE atlas_transporter.ride_booking ALTER COLUMN id SET DEFAULT atlas_transporter.uuid_generate_v4();
ALTER TABLE atlas_transporter.ride ALTER COLUMN id SET DEFAULT atlas_transporter.uuid_generate_v4();
ALTER TABLE atlas_transporter.search_request_location ALTER COLUMN id SET DEFAULT atlas_transporter.uuid_generate_v4();
ALTER TABLE atlas_transporter.organization ALTER COLUMN id SET DEFAULT atlas_transporter.uuid_generate_v4();
ALTER TABLE atlas_transporter.person ALTER COLUMN id SET DEFAULT atlas_transporter.uuid_generate_v4();
ALTER TABLE atlas_transporter.product ALTER COLUMN id SET DEFAULT atlas_transporter.uuid_generate_v4();
ALTER TABLE atlas_transporter.registration_token ALTER COLUMN id SET DEFAULT atlas_transporter.uuid_generate_v4();
ALTER TABLE atlas_transporter.vehicle ALTER COLUMN id SET DEFAULT atlas_transporter.uuid_generate_v4();
ALTER TABLE atlas_transporter.transporter_config ALTER COLUMN id SET DEFAULT atlas_transporter.uuid_generate_v4();
ALTER TABLE atlas_transporter.fare_policy ALTER COLUMN id SET DEFAULT atlas_transporter.uuid_generate_v4();
ALTER TABLE atlas_transporter.notification_status ALTER COLUMN id SET DEFAULT atlas_transporter.uuid_generate_v4();
ALTER TABLE atlas_transporter.allocation_event ALTER COLUMN id SET DEFAULT atlas_transporter.uuid_generate_v4();