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
-- Name: atlas_gateway; Type: SCHEMA; Schema: -; Owner: atlas
--

CREATE SCHEMA atlas_gateway;


ALTER SCHEMA atlas_gateway OWNER TO atlas;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- TOC entry 203 (class 1259 OID 16386)
-- Name: case; Type: TABLE; Schema: atlas_gateway; Owner: atlas
--

CREATE TABLE atlas_gateway."case" (
    id character(36) NOT NULL,
    name character varying(255),
    description character varying(1024),
    short_id character varying(36) NOT NULL,
    industry character varying(1024) NOT NULL,
    type character varying(255) NOT NULL,
    exchange_type character varying(255) NOT NULL,
    status character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    end_time timestamp with time zone,
    valid_till timestamp with time zone NOT NULL,
    gateway character varying(255),
    gateway_type character varying(255),
    requestor character varying(255),
    requestor_type character varying(255),
    parent_case_id character varying(255),
    from_location_id character varying(36),
    to_location_id character varying(36),
    udf1 character varying(255),
    udf2 character varying(255),
    udf3 character varying(255),
    udf4 character varying(255),
    udf5 character varying(255),
    info text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_gateway."case" OWNER TO atlas;

--
-- TOC entry 205 (class 1259 OID 16402)
-- Name: location; Type: TABLE; Schema: atlas_gateway; Owner: atlas
--

CREATE TABLE atlas_gateway.location (
    id character(36) NOT NULL,
    location_type character varying(255),
    lat double precision,
    long double precision,
    ward character varying(255),
    district character varying(255),
    city character varying(255),
    state character varying(255),
    country character varying(255),
    pincode character varying(255),
    address character varying(255),
    bound character varying(255),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_gateway.location OWNER TO atlas;

--
-- TOC entry 206 (class 1259 OID 16410)
-- Name: organization; Type: TABLE; Schema: atlas_gateway; Owner: atlas
--

CREATE TABLE atlas_gateway.organization (
    id character(36) NOT NULL,
    name character varying(255),
    gstin character varying(255),
    status character varying(255),
    type character varying(255),
    domain character varying(255),
    verified boolean NOT NULL,
    enabled boolean DEFAULT true NOT NULL,
    location_id character varying(255),
    description text,
    mobile_number text,
    mobile_country_code character varying(255),
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


ALTER TABLE atlas_gateway.organization OWNER TO atlas;

--
-- TOC entry 2997 (class 0 OID 16386)
-- Dependencies: 203
-- Data for Name: case; Type: TABLE DATA; Schema: atlas_gateway; Owner: atlas
--

COPY atlas_gateway."case" (id, name, description, short_id, industry, type, exchange_type, status, start_time, end_time, valid_till, gateway, gateway_type, requestor, requestor_type, parent_case_id, from_location_id, to_location_id, udf1, udf2, udf3, udf4, udf5, info, created_at, updated_at) FROM stdin;
\.


--
-- TOC entry 2999 (class 0 OID 16402)
-- Dependencies: 205
-- Data for Name: location; Type: TABLE DATA; Schema: atlas_gateway; Owner: atlas
--

COPY atlas_gateway.location (id, location_type, lat, long, ward, district, city, state, country, pincode, address, bound, created_at, updated_at) FROM stdin;
\.


--
-- TOC entry 3000 (class 0 OID 16410)
-- Dependencies: 206
-- Data for Name: organization; Type: TABLE DATA; Schema: atlas_gateway; Owner: atlas
--

COPY atlas_gateway.organization (id, name, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key, info) FROM stdin;
test-provider-2                     	Test provider 2	\N	APPROVED	PROVIDER	FINAL_MILE_DELIVERY	t	t	\N	\N	\N	\N	\N	\N	test-provider-2-key	http://localhost:8017/v1	\N	2020-06-08 18:37:00+00	2020-06-08 18:37:00+00	test-provider-2-key	\N
mobility-provider                   	mobility	\N	APPROVED	PROVIDER	MOBILITY	t	t	\N	\N	\N	\N	\N	\N	mobility-provider-key	http://localhost:8014/v1	\N	2020-06-08 18:37:00+00	2020-06-08 18:37:00+00	mobility-provider-key	\N
provider-wrapper                   	fmd-wrapper	\N	APPROVED	PROVIDER	FINAL_MILE_DELIVERY	t	t	\N	\N	\N	\N	\N	\N	fmd-wrapper-key	http://localhost:8018/v1	\N	2020-08-01 18:37:00+00	2020-08-01 18:37:00+00	test-bpp-key	\N
test-app-2                          	Test App 2	\N	APPROVED	APP	FINAL_MILE_DELIVERY	t	t	\N	\N	\N	\N	\N	\N	test-app-2-key	http://localhost:8016/v1	\N	2020-06-08 18:37:00+00	2020-06-08 18:37:00+00	\N	\N
mobility-app                        	mobility	\N	APPROVED	APP	MOBILITY	t	t	\N	\N	\N	\N	\N	\N	mobility-app-key	http://localhost:8013/v1	\N	2020-06-08 18:37:00+00	2020-06-08 18:37:00+00	\N	\N
\.

--
-- TOC entry 2837 (class 2606 OID 16460)
-- Name: case idx_16386_primary; Type: CONSTRAINT; Schema: atlas_gateway; Owner: atlas
--

ALTER TABLE ONLY atlas_gateway."case"
    ADD CONSTRAINT idx_16386_primary PRIMARY KEY (id);


--
-- TOC entry 2849 (class 2606 OID 16466)
-- Name: location idx_16402_primary; Type: CONSTRAINT; Schema: atlas_gateway; Owner: atlas
--

ALTER TABLE ONLY atlas_gateway.location
    ADD CONSTRAINT idx_16402_primary PRIMARY KEY (id);


--
-- TOC entry 2852 (class 2606 OID 16468)
-- Name: organization idx_16410_primary; Type: CONSTRAINT; Schema: atlas_gateway; Owner: atlas
--

ALTER TABLE ONLY atlas_gateway.organization
    ADD CONSTRAINT idx_16410_primary PRIMARY KEY (id);


ALTER TABLE ONLY atlas_gateway.organization
  ADD CONSTRAINT unique_api_key UNIQUE (api_key);

--
-- TOC entry 2838 (class 1259 OID 16481)
-- Name: idx_16386_gateway; Type: INDEX; Schema: atlas_gateway; Owner: atlas
--

CREATE INDEX idx_16386_gateway ON atlas_gateway."case" USING btree (gateway);


--
-- TOC entry 2839 (class 1259 OID 16482)
-- Name: idx_16386_requestor; Type: INDEX; Schema: atlas_gateway; Owner: atlas
--

CREATE INDEX idx_16386_requestor ON atlas_gateway."case" USING btree (requestor);


--
-- TOC entry 2840 (class 1259 OID 16483)
-- Name: idx_16386_short_id; Type: INDEX; Schema: atlas_gateway; Owner: atlas
--

CREATE UNIQUE INDEX idx_16386_short_id ON atlas_gateway."case" USING btree (short_id);

--
-- TOC entry 2847 (class 1259 OID 16490)
-- Name: idx_16402_city; Type: INDEX; Schema: atlas_gateway; Owner: atlas
--

CREATE INDEX idx_16402_city ON atlas_gateway.location USING btree (city);


--
-- TOC entry 2850 (class 1259 OID 16491)
-- Name: idx_16402_state; Type: INDEX; Schema: atlas_gateway; Owner: atlas
--

CREATE INDEX idx_16402_state ON atlas_gateway.location USING btree (state);

-- Completed on 2020-07-20 19:25:37 IST

--
-- PostgreSQL database dump complete
--

CREATE TABLE atlas_gateway.trail (
    id character(36) NOT NULL,
    --customer_id character(36),
    --session_id character(36),
    endpoint_id character varying(64) NOT NULL,
    headers text NOT NULL,
    query_params text NOT NULL,
    remote_host text NOT NULL,
    request_body text,  -- TODO: do we want to limit size of request somehow?
    is_secure boolean NOT NULL,
    succeeded boolean,
    response_status text,
    response_body text,  -- TODO: do we want to limit size of response somehow?
                         -- reponse usually contains a list, so data can be infinitely large
    response_headers text,  -- TODO: limit them?
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    process_duration int
);

ALTER TABLE ONLY atlas_gateway."trail"
    ADD CONSTRAINT idx_trail_primary PRIMARY KEY (id);



CREATE TABLE atlas_gateway.external_trail (
    id character(36) NOT NULL,
    --customer_id character(36),
    --session_id character(36),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    gateway_id character varying(16) NOT NULL,
    endpoint_id character varying(16) NOT NULL,
    headers text NOT NULL,
    query_params text NOT NULL,
    request text,  -- TODO: do we want to limit size of request somehow?
    succeeded boolean,
    response text,  -- TODO: do we want to limit size of response somehow?
                    -- reponse usually contains a list, so data can be infinitely large
    error text
);

ALTER TABLE ONLY atlas_gateway."external_trail"
    ADD CONSTRAINT idx_external_trail_primary PRIMARY KEY (id);
