--
-- PostgreSQL database dump
--

-- Dumped from database version 12.3 (Debian 12.3-1.pgdg100+1)
-- Dumped by pg_dump version 12.3

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
-- Name: atlas_transporter; Type: SCHEMA; Schema: -; Owner: atlas
--

CREATE SCHEMA atlas_transporter;


ALTER SCHEMA atlas_transporter OWNER TO atlas;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: case; Type: TABLE; Schema: atlas_transporter; Owner: atlas
--

CREATE TABLE atlas_transporter.case (
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
    provider character varying(255),
    provider_type character varying(255),
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


ALTER TABLE atlas_transporter.case OWNER TO atlas;

--
-- Name: product_instance; Type: TABLE; Schema: atlas_transporter; Owner: atlas
--

CREATE TABLE atlas_transporter.product_instance (
    id character(36) NOT NULL,
    case_id character varying(255) NOT NULL,
    product_id character varying(255) NOT NULL,
    person_id character varying(255),
    quantity bigint NOT NULL,
    price numeric(8,2) NOT NULL,
    status character varying(255) NOT NULL,
    info text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_transporter.product_instance OWNER TO atlas;

--
-- Name: location; Type: TABLE; Schema: atlas_transporter; Owner: atlas
--

CREATE TABLE atlas_transporter.location (
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


ALTER TABLE atlas_transporter.location OWNER TO atlas;

--
-- Name: organization; Type: TABLE; Schema: atlas_transporter; Owner: atlas
--

CREATE TABLE atlas_transporter.organization (
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
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_transporter.organization OWNER TO atlas;

--
-- Name: person; Type: TABLE; Schema: atlas_transporter; Owner: atlas
--

CREATE TABLE atlas_transporter.person (
    id character(36) NOT NULL,
    first_name character varying(255),
    middle_name character varying(255),
    last_name character varying(255),
    full_name character varying(255),
    role character varying(255) NOT NULL,
    gender character varying(255) NOT NULL,
    identifier_type character varying(255),
    email character varying(255),
    mobile_number character varying(255),
    mobile_country_code character varying(255),
    identifier character varying(255),
    rating character varying(255),
    verified boolean NOT NULL,
    udf1 character varying(255),
    udf2 character varying(255),
    status character varying(255) NOT NULL,
    organization_id character varying(255),
    device_token character varying(255),
    location_id character varying(255),
    description character varying(255),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_transporter.person OWNER TO atlas;

--
-- Name: product; Type: TABLE; Schema: atlas_transporter; Owner: atlas
--

CREATE TABLE atlas_transporter.product (
    id character(36) NOT NULL,
    name character varying(255),
    short_id character varying(36) NOT NULL,
    description character varying(1024),
    industry character varying(1024) NOT NULL,
    type character varying(255) NOT NULL,
    status character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    end_time timestamp with time zone,
    valid_till timestamp with time zone NOT NULL,
    price numeric(10,2) NOT NULL,
    rating character varying(255),
    review character varying(255),
    udf1 character varying(255),
    udf2 character varying(255),
    udf3 character varying(255),
    udf4 character varying(255),
    udf5 character varying(255),
    info text,
    from_location_id character varying(255),
    to_location_id character varying(255),
    organization_id character varying(255) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    assigned_to character(36)
);


ALTER TABLE atlas_transporter.product OWNER TO atlas;

--
-- Name: registration_token; Type: TABLE; Schema: atlas_transporter; Owner: atlas
--

CREATE TABLE atlas_transporter.registration_token (
    id character(36) NOT NULL,
    auth_medium character varying(255) NOT NULL,
    auth_type character varying(255) NOT NULL,
    auth_value_hash character varying(1024) NOT NULL,
    token character varying(1024) NOT NULL,
    verified boolean NOT NULL,
    auth_expiry bigint NOT NULL,
    token_expiry bigint NOT NULL,
    attempts bigint NOT NULL,
    entity_id character(36) NOT NULL,
    entity_type character(36) NOT NULL,
    info text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_transporter.registration_token OWNER TO atlas;

--
-- Name: vehicle; Type: TABLE; Schema: atlas_transporter; Owner: atlas
--

CREATE TABLE atlas_transporter.vehicle (
    id character(36) NOT NULL,
    capacity bigint,
    category character varying(255),
    make character varying(255),
    model character varying(255),
    size character varying(255),
    variant character varying(255),
    color character varying(255),
    energy_type character varying(255),
    registration_no character varying(255) NOT NULL,
    registration_category character varying(255),
    organization_id character(36),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_transporter.vehicle OWNER TO atlas;

--
-- Data for Name: case; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

COPY atlas_transporter.case (id, name, description, short_id, industry, type, exchange_type, status, start_time, end_time, valid_till, provider, provider_type, requestor, requestor_type, parent_case_id, from_location_id, to_location_id, udf1, udf2, udf3, udf4, udf5, info, created_at, updated_at) FROM stdin;
\.


--
-- Data for Name: product_instance; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

COPY atlas_transporter.product_instance (id, case_id, product_id, person_id, quantity, price, status, info, created_at, updated_at) FROM stdin;
\.


--
-- Data for Name: location; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

COPY atlas_transporter.location (id, location_type, lat, long, ward, district, city, state, country, pincode, address, bound, created_at, updated_at) FROM stdin;
\.


--
-- Data for Name: organization; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

COPY atlas_transporter.organization (id, name, gstin, status, type, verified, enabled, location_id, description, mobile_number, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at) FROM stdin;
1926d40f-1223-4eb2-ba5d-7983bde2fd02	juspay	\N	PENDING_VERIFICATION	GATEWAY	t	t	\N	\N	\N	\N	\N	iamfromjuspay	\N	\N	2020-06-08 18:37:00+00	2020-06-08 18:37:00+00
\.


--
-- Data for Name: person; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

COPY atlas_transporter.person (id, first_name, middle_name, last_name, full_name, role, gender, identifier_type, email, mobile_number, mobile_country_code, identifier, rating, verified, udf1, udf2, status, organization_id, device_token, location_id, description, created_at, updated_at) FROM stdin;
ec34eede-5a3e-4a41-89d4-7290a0d7a629	\N	\N	\N	\N	ADMIN	UNKNOWN	MOBILENUMBER	\N	+919999999999	\N	+919999999999	\N	t	\N	\N	INACTIVE	1926d40f-1223-4eb2-ba5d-7983bde2fd02	\N	\N	\N	2020-06-08 18:37:00+00	2020-06-08 18:37:00+00
\.


--
-- Data for Name: product; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

COPY atlas_transporter.product (id, name, short_id, description, industry, type, status, start_time, end_time, valid_till, price, rating, review, udf1, udf2, udf3, udf4, udf5, info, from_location_id, to_location_id, organization_id, created_at, updated_at, assigned_to) FROM stdin;
\.


--
-- Data for Name: registration_token; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

COPY atlas_transporter.registration_token (id, auth_medium, auth_type, auth_value_hash, token, verified, auth_expiry, token_expiry, attempts, entity_id, entity_type, info, created_at, updated_at) FROM stdin;
772453e2-d02b-494a-a4ac-ec1ea0027e18	SMS	OTP	3249	ea37f941-427a-4085-a7d0-96240f166672	t	3	365	3	ec34eede-5a3e-4a41-89d4-7290a0d7a629	USER                                	\N	2020-06-08 18:37:00+00	2020-06-08 18:37:00+00
\.


--
-- Data for Name: vehicle; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

COPY atlas_transporter.vehicle (id, capacity, category, make, model, size, variant, color, energy_type, registration_no, registration_category, organization_id, created_at, updated_at) FROM stdin;
\.


--
-- Name: case idx_16386_primary; Type: CONSTRAINT; Schema: atlas_transporter; Owner: atlas
--

ALTER TABLE ONLY atlas_transporter.case
    ADD CONSTRAINT idx_16386_primary PRIMARY KEY (id);


--
-- Name: product_instance idx_16394_primary; Type: CONSTRAINT; Schema: atlas_transporter; Owner: atlas
--

ALTER TABLE ONLY atlas_transporter.product_instance
    ADD CONSTRAINT idx_16394_primary PRIMARY KEY (id);


--
-- Name: location idx_16402_primary; Type: CONSTRAINT; Schema: atlas_transporter; Owner: atlas
--

ALTER TABLE ONLY atlas_transporter.location
    ADD CONSTRAINT idx_16402_primary PRIMARY KEY (id);


--
-- Name: organization idx_16410_primary; Type: CONSTRAINT; Schema: atlas_transporter; Owner: atlas
--

ALTER TABLE ONLY atlas_transporter.organization
    ADD CONSTRAINT idx_16410_primary PRIMARY KEY (id);


--
-- Name: person idx_16419_primary; Type: CONSTRAINT; Schema: atlas_transporter; Owner: atlas
--

ALTER TABLE ONLY atlas_transporter.person
    ADD CONSTRAINT idx_16419_primary PRIMARY KEY (id);


--
-- Name: product idx_16427_primary; Type: CONSTRAINT; Schema: atlas_transporter; Owner: atlas
--

ALTER TABLE ONLY atlas_transporter.product
    ADD CONSTRAINT idx_16427_primary PRIMARY KEY (id);


--
-- Name: registration_token idx_16435_primary; Type: CONSTRAINT; Schema: atlas_transporter; Owner: atlas
--

ALTER TABLE ONLY atlas_transporter.registration_token
    ADD CONSTRAINT idx_16435_primary PRIMARY KEY (id);


--
-- Name: idx_16386_provider; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16386_provider ON atlas_transporter.case USING btree (provider);


--
-- Name: idx_16386_requestor; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16386_requestor ON atlas_transporter.case USING btree (requestor);


--
-- Name: idx_16386_short_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE UNIQUE INDEX idx_16386_short_id ON atlas_transporter.case USING btree (short_id);


--
-- Name: idx_16394_case_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16394_case_id ON atlas_transporter.product_instance USING btree (case_id);


--
-- Name: idx_16394_product_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16394_product_id ON atlas_transporter.product_instance USING btree (product_id);


--
-- Name: idx_16402_city; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16402_city ON atlas_transporter.location USING btree (city);


--
-- Name: idx_16402_state; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16402_state ON atlas_transporter.location USING btree (state);


--
-- Name: idx_16427_organization_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16427_organization_id ON atlas_transporter.product USING btree (organization_id);


--
-- Name: idx_16427_short_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16427_short_id ON atlas_transporter.product USING btree (short_id);


--
-- Name: idx_16435_entity_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16435_entity_id ON atlas_transporter.registration_token USING btree (entity_id);


--
-- Name: idx_16435_entity_type; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16435_entity_type ON atlas_transporter.registration_token USING btree (entity_type);


--
-- PostgreSQL database dump complete
--

