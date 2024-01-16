-- auxiliary settings
CREATE USER atlas_registry_user WITH PASSWORD 'atlas';
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
CREATE SCHEMA atlas_registry;
ALTER SCHEMA atlas_registry OWNER TO atlas_registry_user;

-- tables






CREATE TABLE atlas_registry.subscriber (
unique_key_id character varying(255) NOT NULL,
subscriber_id character varying(255) NOT NULL,
subscriber_url character varying(255) NOT NULL,
type character varying(255) NOT NULL,
domain character varying(255) NOT NULL,
city text [],
country character varying(255),
status character varying(255),
signing_public_key character varying(255) NOT NULL,
encr_public_key character varying(255),
valid_from timestamp with time zone,
valid_until timestamp with time zone,
created timestamp with time zone DEFAULT now() NOT NULL,
updated timestamp with time zone DEFAULT now() NOT NULL
,CONSTRAINT  subscriber_pkey PRIMARY KEY (unique_key_id, subscriber_id)
);
ALTER TABLE atlas_registry.subscriber OWNER TO atlas_registry_user;

-- necessary data

-- thank you for getting here!
