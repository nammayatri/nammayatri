CREATE USER atlas_person_location_user WITH PASSWORD 'atlas';
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
CREATE SCHEMA atlas_person_location;
ALTER SCHEMA atlas_person_location OWNER TO atlas_person_location_user;

CREATE TABLE atlas_person_location.driver_location (
driver_id character(36) NOT NULL,
lat double precision NOT NULL,
lon double precision NOT NULL,
point public.geography(Point,4326) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
coordinates_calculated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
merchant_id character(36) NOT NULL
,CONSTRAINT  driver_location_pkey PRIMARY KEY (driver_id)
);
ALTER TABLE atlas_person_location.driver_location OWNER TO atlas_person_location_user;
