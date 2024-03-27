-- auxiliary settings
CREATE USER atlas_safety_dashboard_user WITH PASSWORD 'atlas';
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
CREATE SCHEMA atlas_safety_dashboard;
ALTER SCHEMA atlas_safety_dashboard OWNER TO atlas_safety_dashboard_user;

-- tables

CREATE TABLE atlas_safety_dashboard.person (
id character(36) NOT NULL,
first_name character varying(255) NOT NULL,
last_name character varying(255) NOT NULL,
email_encrypted character varying(255) NOT NULL,
email_hash bytea NOT NULL,
mobile_number_encrypted character varying(255) NOT NULL,
mobile_number_hash bytea NOT NULL,
mobile_country_code character varying(255) NOT NULL,
password_hash bytea NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16451_primary PRIMARY KEY (id)
,CONSTRAINT  unique_email UNIQUE (email_hash)
,CONSTRAINT  unique_mobile_number_country_code UNIQUE (mobile_country_code, mobile_number_hash)
);
ALTER TABLE atlas_safety_dashboard.person OWNER TO atlas_safety_dashboard_user;

CREATE TABLE atlas_safety_dashboard.registration_token (
id character(36) NOT NULL,
token character varying(1024) NOT NULL,
person_id character(36) REFERENCES atlas_safety_dashboard.person (id) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16467_primary PRIMARY KEY (id)
);
ALTER TABLE atlas_safety_dashboard.registration_token OWNER TO atlas_safety_dashboard_user;
CREATE INDEX idx_16467_person_id ON atlas_safety_dashboard.registration_token USING btree (person_id);
