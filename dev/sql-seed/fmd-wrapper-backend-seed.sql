--
-- PostgreSQL database dump
--

-- Dumped from database version 12.3 (Debian 12.3-1.pgdg100+1)
-- Dumped by pg_dump version 12.3

-- Started on 2020-07-20 19:25:37 IST

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

--
-- TOC entry 8 (class 2615 OID 16385)
-- Name: atlas_fmd_wrapper; Type: SCHEMA; Schema: -; Owner: atlas
--

CREATE SCHEMA atlas_fmd_wrapper;


ALTER SCHEMA atlas_fmd_wrapper OWNER TO atlas;

SET default_tablespace = '';

SET default_table_access_method = heap;

CREATE USER atlas_fmd_wrapper_user WITH PASSWORD 'atlas';
ALTER SCHEMA atlas_fmd_wrapper OWNER TO atlas_fmd_wrapper_user;

--
-- TOC entry 203 (class 1259 OID 16386)
-- Name: case; Type: TABLE; Schema: atlas_fmd_wrapper; Owner: atlas
--

CREATE TABLE atlas_fmd_wrapper.case (
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


ALTER TABLE atlas_fmd_wrapper.case OWNER TO atlas_fmd_wrapper_user;
--
-- TOC entry 206 (class 1259 OID 16410)
-- Name: organization; Type: TABLE; Schema: atlas_fmd_wrapper; Owner: atlas
--

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
    callback_url text,
    head_count bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    callback_api_key text,
    info text
);

ALTER TABLE atlas_fmd_wrapper.organization OWNER TO atlas_fmd_wrapper_user;
--
-- TOC entry 2852 (class 2606 OID 16468)
-- Name: organization idx_16410_primary; Type: CONSTRAINT; Schema: atlas_fmd_wrapper; Owner: atlas
--

ALTER TABLE ONLY atlas_fmd_wrapper.organization
    ADD CONSTRAINT idx_16410_primary PRIMARY KEY (id);

ALTER TABLE ONLY atlas_fmd_wrapper.organization
  ADD CONSTRAINT unique_api_key UNIQUE (api_key);

ALTER TABLE ONLY atlas_fmd_wrapper.organization
  ADD CONSTRAINT unique_callback_url UNIQUE (callback_url);

ALTER TABLE ONLY atlas_fmd_wrapper.organization
  ADD CONSTRAINT unique_short_id UNIQUE (short_id);


CREATE INDEX idx_organization_short_id ON atlas_fmd_wrapper.organization USING btree (short_id);

-- Completed on 2020-07-20 19:25:37 IST

--
-- PostgreSQL database dump complete
--
