-- auxiliary settings
CREATE USER atlas_fmd_wrapper_user WITH PASSWORD 'atlas';
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
CREATE SCHEMA atlas_fmd_wrapper;
ALTER SCHEMA atlas_fmd_wrapper OWNER TO atlas_fmd_wrapper_user;

-- tables






CREATE TABLE atlas_fmd_wrapper.organization (
id character(36) NOT NULL,
name character varying(255),
short_id character varying(255) NOT NULL,
gstin character varying(255),
status character varying(255),
type character varying(255),
domain character varying(255),
verified boolean NOT NULL,
enabled boolean DEFAULT true NOT NULL,
location_id character varying(255),
description text,
mobile_number text,
mobile_country_code text,
from_time timestamp with time zone,
to_time timestamp with time zone,
api_key text,
head_count bigint,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
info text
,CONSTRAINT  idx_16410_primary PRIMARY KEY (id)
,CONSTRAINT  unique_api_key UNIQUE (api_key)
,CONSTRAINT  unique_short_id UNIQUE (short_id)
);
ALTER TABLE atlas_fmd_wrapper.organization OWNER TO atlas_fmd_wrapper_user;
CREATE INDEX idx_organization_short_id ON atlas_fmd_wrapper.organization USING btree (short_id);







CREATE TABLE atlas_fmd_wrapper.search_request (
id character(36) NOT NULL,
name character varying(255),
description character varying(1024),
short_id character varying(36),
industry character varying(1024) NOT NULL,
type character varying(255) NOT NULL,
exchange_type character varying(255) NOT NULL,
status character varying(255) NOT NULL,
start_time timestamp with time zone NOT NULL,
end_time timestamp with time zone,
valid_till timestamp with time zone NOT NULL,
provider character varying(255),
provider_type character varying(255),
requestor character varying(255),
requestor_type character varying(255),
parent_case_id character varying(255),
from_location_id character varying(36),
to_location_id character varying(36),
udf1 text,
udf2 text,
udf3 text,
udf4 text,
udf5 text,
info text,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
ALTER TABLE atlas_fmd_wrapper.search_request OWNER TO atlas_fmd_wrapper_user;

-- necessary data

-- thank you for getting here!
