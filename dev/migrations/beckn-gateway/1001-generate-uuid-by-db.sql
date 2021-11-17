CREATE OR REPLACE FUNCTION atlas_gateway.uuid_generate_v4() RETURNS character (36)
    AS 'SELECT uuid_in(overlay(overlay(md5(random()::text || '':'' || clock_timestamp()::text) placing ''4'' from 13) placing to_hex(floor(random()*(11-8+1) + 8)::int)::text from 17)::cstring)::character (36);'
    LANGUAGE SQL
    IMMUTABLE;

ALTER TABLE atlas_gateway.search_request ALTER COLUMN id SET DEFAULT atlas_gateway.uuid_generate_v4();
ALTER TABLE atlas_gateway.location ALTER COLUMN id SET DEFAULT atlas_gateway.uuid_generate_v4();
ALTER TABLE atlas_gateway.organization ALTER COLUMN id SET DEFAULT atlas_gateway.uuid_generate_v4();