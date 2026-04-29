CREATE FUNCTION atlas_driver_offer_bpp.uuid_generate_v4() RETURNS character
    LANGUAGE sql IMMUTABLE
    AS $$SELECT uuid_in(overlay(overlay(md5(random()::text
        || ':' || clock_timestamp()::text) placing '4' from 13)
        placing to_hex(floor(random()*(11-8+1) + 8)::int)::text from 17)::cstring)::character (36);$$;

CREATE TABLE atlas_driver_offer_bpp.geometry (
    id character(36) DEFAULT atlas_driver_offer_bpp.uuid_generate_v4() NOT NULL,
    region character varying(255) NOT NULL,
    geom public.geometry(MultiPolygon),
    CONSTRAINT geometry_pkey PRIMARY KEY (id)
);