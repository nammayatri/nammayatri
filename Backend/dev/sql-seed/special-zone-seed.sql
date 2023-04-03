CREATE USER atlas_special_zone_user WITH PASSWORD 'atlas';
SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;
CREATE SCHEMA atlas_special_zone;
ALTER SCHEMA atlas_special_zone OWNER TO atlas_special_zone_user;

CREATE TABLE atlas_special_zone.special_zone (
id character(36) NOT NULL PRIMARY KEY,
name character varying(255) NOT NULL,
category_code character varying(30) NOT NULL,
geom public.geometry(MultiPolygon),
geo_json text,
city character varying(20),
state character varying(20),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_special_zone.special_zone OWNER TO atlas_special_zone_user;

CREATE TABLE atlas_special_zone.tag_category_mapping (
id character(36) NOT NULL,
tag character varying(255) NOT NULL PRIMARY KEY,
category_code character varying(30) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_special_zone.tag_category_mapping OWNER TO atlas_special_zone_user;

CREATE TABLE atlas_special_zone.entry_exit (
id character(36) NOT NULL,
special_zone_id character(36) NOT NULL REFERENCES atlas_special_zone.special_zone (id),
entry_exit_type character varying(20) NOT NULL,
lat double precision NOT NULL,
lon double precision NOT NULL,
area character varying(255),
address character varying(255),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_special_zone.entry_exit OWNER TO atlas_special_zone_user;

