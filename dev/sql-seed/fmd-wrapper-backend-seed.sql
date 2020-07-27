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

--
-- TOC entry 203 (class 1259 OID 16386)
-- Name: case; Type: TABLE; Schema: atlas_fmd_wrapper; Owner: atlas
--

CREATE TABLE atlas_fmd_wrapper."case" (
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


ALTER TABLE atlas_fmd_wrapper."case" OWNER TO atlas;

--
-- TOC entry 206 (class 1259 OID 16410)
-- Name: organization; Type: TABLE; Schema: atlas_fmd_wrapper; Owner: atlas
--

CREATE TABLE atlas_fmd_wrapper.organization (
    id character(36) NOT NULL,
    name character varying(255),
    gstin character varying(255),
    status character varying(255),
    type character varying(255),
    verified boolean NOT NULL,
    enabled boolean DEFAULT true NOT NULL,
    location_id character varying(255),
    description text,
    mobile_number text,
    from_time timestamp with time zone,
    to_time timestamp with time zone,
    api_key text,
    callback_url text,
    head_count bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    callback_api_key text
);


ALTER TABLE atlas_fmd_wrapper.organization OWNER TO atlas;

--
-- TOC entry 2997 (class 0 OID 16386)
-- Dependencies: 203
-- Data for Name: case; Type: TABLE DATA; Schema: atlas_fmd_wrapper; Owner: atlas
--

COPY atlas_fmd_wrapper."case" (id, name, description, short_id, industry, type, exchange_type, status, start_time, end_time, valid_till, gateway, gateway_type, requestor, requestor_type, parent_case_id, from_location_id, to_location_id, udf1, udf2, udf3, udf4, udf5, info, created_at, updated_at) FROM stdin;
\.

--
-- TOC entry 3000 (class 0 OID 16410)
-- Dependencies: 206
-- Data for Name: organization; Type: TABLE DATA; Schema: atlas_fmd_wrapper; Owner: atlas
--

COPY atlas_fmd_wrapper.organization (id, name, gstin, status, type, verified, enabled, location_id, description, mobile_number, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at, callback_api_key) FROM stdin;
test-provider-2                     	Test provider 2	\N	APPROVED	PROVIDER	t	t	\N	\N	\N	\N	\N	test-provider-2-key	http://localhost:8017/v1	\N	2020-06-08 18:37:00+00	2020-06-08 18:37:00+00	\N
mobility-provider                   	mobility	\N	APPROVED	PROVIDER	t	t	\N	\N	\N	\N	\N	mobility-provider-key	http://localhost:8014/v1	\N	2020-06-08 18:37:00+00	2020-06-08 18:37:00+00	\N
test-app-2                          	Test App 2	\N	APPROVED	APP	t	t	\N	\N	\N	\N	\N	test-app-2-key	http://localhost:8016/v1	\N	2020-06-08 18:37:00+00	2020-06-08 18:37:00+00	\N
mobility-app                        	mobility	\N	APPROVED	APP	t	t	\N	\N	\N	\N	\N	mobility-app-key	http://localhost:8014/v1	\N	2020-06-08 18:37:00+00	2020-06-08 18:37:00+00	\N
\.

--
-- TOC entry 2837 (class 2606 OID 16460)
-- Name: case idx_16386_primary; Type: CONSTRAINT; Schema: atlas_fmd_wrapper; Owner: atlas
--

ALTER TABLE ONLY atlas_fmd_wrapper."case"
    ADD CONSTRAINT idx_16386_primary PRIMARY KEY (id);


--
-- TOC entry 2852 (class 2606 OID 16468)
-- Name: organization idx_16410_primary; Type: CONSTRAINT; Schema: atlas_fmd_wrapper; Owner: atlas
--

ALTER TABLE ONLY atlas_fmd_wrapper.organization
    ADD CONSTRAINT idx_16410_primary PRIMARY KEY (id);


--
-- TOC entry 2838 (class 1259 OID 16481)
-- Name: idx_16386_gateway; Type: INDEX; Schema: atlas_fmd_wrapper; Owner: atlas
--

CREATE INDEX idx_16386_gateway ON atlas_fmd_wrapper."case" USING btree (gateway);


--
-- TOC entry 2839 (class 1259 OID 16482)
-- Name: idx_16386_requestor; Type: INDEX; Schema: atlas_fmd_wrapper; Owner: atlas
--

CREATE INDEX idx_16386_requestor ON atlas_fmd_wrapper."case" USING btree (requestor);


--
-- TOC entry 2840 (class 1259 OID 16483)
-- Name: idx_16386_short_id; Type: INDEX; Schema: atlas_fmd_wrapper; Owner: atlas
--

CREATE UNIQUE INDEX idx_16386_short_id ON atlas_fmd_wrapper."case" USING btree (short_id);


-- Completed on 2020-07-20 19:25:37 IST

--
-- PostgreSQL database dump complete
--
