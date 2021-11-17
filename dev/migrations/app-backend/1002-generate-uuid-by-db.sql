CREATE OR REPLACE FUNCTION atlas_app.uuid_generate_v4() RETURNS character (36)
    AS 'SELECT uuid_in(overlay(overlay(md5(random()::text || '':'' || clock_timestamp()::text) placing ''4'' from 13) placing to_hex(floor(random()*(11-8+1) + 8)::int)::text from 17)::cstring)::character (36);'
    LANGUAGE SQL
    IMMUTABLE;

ALTER TABLE atlas_app.search_request ALTER COLUMN id SET DEFAULT atlas_app.uuid_generate_v4();
ALTER TABLE atlas_app.quote ALTER COLUMN id SET DEFAULT atlas_app.uuid_generate_v4();
ALTER TABLE atlas_app.ride_booking ALTER COLUMN id SET DEFAULT atlas_app.uuid_generate_v4();
ALTER TABLE atlas_app.ride ALTER COLUMN id SET DEFAULT atlas_app.uuid_generate_v4();
ALTER TABLE atlas_app.comment ALTER COLUMN id SET DEFAULT atlas_app.uuid_generate_v4();
ALTER TABLE atlas_app.document ALTER COLUMN id SET DEFAULT atlas_app.uuid_generate_v4();
ALTER TABLE atlas_app.entity_document ALTER COLUMN id SET DEFAULT atlas_app.uuid_generate_v4();
ALTER TABLE atlas_app.entity_tag ALTER COLUMN id SET DEFAULT atlas_app.uuid_generate_v4();
ALTER TABLE atlas_app.search_request_location ALTER COLUMN id SET DEFAULT atlas_app.uuid_generate_v4();
ALTER TABLE atlas_app.organization ALTER COLUMN id SET DEFAULT atlas_app.uuid_generate_v4();
ALTER TABLE atlas_app.person ALTER COLUMN id SET DEFAULT atlas_app.uuid_generate_v4();
ALTER TABLE atlas_app.registration_token ALTER COLUMN id SET DEFAULT atlas_app.uuid_generate_v4();
ALTER TABLE atlas_app.tag ALTER COLUMN id SET DEFAULT atlas_app.uuid_generate_v4();
ALTER TABLE atlas_app.issues ALTER COLUMN id SET DEFAULT atlas_app.uuid_generate_v4();