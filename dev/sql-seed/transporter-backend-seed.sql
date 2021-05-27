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
    short_id character varying(255) NOT NULL,
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
    person_updated_at timestamp with time zone,
    short_id character varying(36) NOT NULL,
    entity_id character varying(255),
    entity_type character varying(255) NOT NULL,
    quantity bigint NOT NULL,
    price numeric(30,10),
    type character varying(255) NOT NULL,
    status character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    end_time timestamp with time zone,
    valid_till timestamp with time zone NOT NULL,
    from_location_id character varying(255),
    to_location_id character varying(255),
    organization_id character varying(255) NOT NULL,
    parent_id character varying(255),
    info text,
    udf1 character varying(255),
    udf2 character varying(255),
    udf3 character varying(255),
    udf4 character varying(255),
    udf5 character varying(255),
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
    point public.geography(POINT,4326),
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

CREATE INDEX location_idx ON atlas_transporter.location USING gist (point);

ALTER TABLE atlas_transporter.location OWNER TO atlas;

--
-- Name: organization; Type: TABLE; Schema: atlas_transporter; Owner: atlas
--

CREATE TABLE atlas_transporter.organization (
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
    mobile_country_code character varying(255),
    from_time timestamp with time zone,
    to_time timestamp with time zone,
    api_key text,
    callback_url text,
    callback_api_key text,
    head_count bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    info text
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
    identifier_type character varying(255) NOT NULL,
    email character varying(255),
    password_hash bytea,
    mobile_number_encrypted character varying(255),
    mobile_number_hash bytea,
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
    name character varying(255) NOT NULL,
    description character varying(1024),
    industry character varying(1024) NOT NULL,
    type character varying(255) NOT NULL,
    rating character varying(255),
    status character varying(255) NOT NULL,
    short_id character(36) NOT NULL,
    price numeric(30,10) NOT NULL,
    review character varying(255),
    udf1 character varying(255),
    udf2 character varying(255),
    udf3 character varying(255),
    udf4 character varying(255),
    udf5 character varying(255),
    info text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
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
-- Name: vehicle; Type: TABLE; Schema: atlas_transporter; Owner: atlas
--

CREATE TABLE atlas_transporter.inventory (
    id character(36) NOT NULL,
    organization_id character varying(255),
    product_id character varying(1024),
    status character varying(255) NOT NULL,
    quantity character varying(255),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE atlas_transporter.inventory OWNER TO atlas;

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
-- Name: inventory idx_16443_primary; Type: CONSTRAINT; Schema: atlas_transporter; Owner: atlas
--

ALTER TABLE ONLY atlas_transporter.inventory
    ADD CONSTRAINT idx_16443_primary PRIMARY KEY (id);


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
-- Name: vehicle idx_16451_primary; Type: CONSTRAINT; Schema: atlas_transporter; Owner: atlas
--

ALTER TABLE ONLY atlas_transporter.vehicle
    ADD CONSTRAINT idx_16451_primary PRIMARY KEY (id);

--
-- Name: person idx_16419_primary; Type: CONSTRAINT; Schema: atlas_transporter; Owner: atlas
--

ALTER TABLE ONLY atlas_transporter.person
    ADD CONSTRAINT idx_16419_primary PRIMARY KEY (id);

ALTER TABLE ONLY atlas_transporter.person
  ADD CONSTRAINT person_unique_mobile_number_country_code UNIQUE (mobile_country_code, mobile_number_hash);

ALTER TABLE ONLY atlas_transporter.person
  ADD CONSTRAINT unique_identifier UNIQUE (identifier);

ALTER TABLE ONLY atlas_transporter.person
  ADD CONSTRAINT unique_email UNIQUE (email);

ALTER TABLE ONLY atlas_transporter.organization
  ADD CONSTRAINT unique_api_key UNIQUE (api_key);

ALTER TABLE ONLY atlas_transporter.organization
  ADD CONSTRAINT unique_short_id UNIQUE (short_id);

ALTER TABLE ONLY atlas_transporter.organization
  ADD CONSTRAINT organization_unique_mobile_number_country_code UNIQUE (mobile_country_code, mobile_number);

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
-- Name: idx_16386_short_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE UNIQUE INDEX idx_16386_short_id ON atlas_transporter.case USING btree (short_id);


ALTER TABLE ONLY atlas_transporter.vehicle
  ADD CONSTRAINT unique_registration_no UNIQUE (registration_no);

--
-- Name: idx_16386_parent_case_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16386_parent_case_id ON atlas_transporter.case USING btree (parent_case_id);


--
-- Name: idx_16386_status; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16386_status ON atlas_transporter."case" USING btree (status);


--
-- Name: idx_16386_type; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16386_type ON atlas_transporter."case" USING btree (type);


--
-- Name: idx_16419_organization_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16419_organization_id ON atlas_transporter.person USING btree (organization_id);


--
-- Name: idx_16419_role; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16419_role ON atlas_transporter.person USING btree (role);


--
-- Name: idx_16394_case_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16394_case_id ON atlas_transporter.product_instance USING btree (case_id);


--
-- Name: idx_16394_entity_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16394_entity_id ON atlas_transporter.product_instance USING btree (entity_id);


--
-- Name: idx_16394_organization_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16394_organization_id ON atlas_transporter.product_instance USING btree (organization_id);


--
-- Name: idx_16394_parent_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16394_parent_id ON atlas_transporter.product_instance USING btree (parent_id);


--
-- Name: idx_16394_person_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16394_person_id ON atlas_transporter.product_instance USING btree (person_id);


--
-- Name: idx_16394_product_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16394_product_id ON atlas_transporter.product_instance USING btree (product_id);


--
-- Name: idx_16394_status; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16394_status ON atlas_transporter.product_instance USING btree (status);


--
-- Name: idx_16394_type; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16394_type ON atlas_transporter.product_instance USING btree (type);


--
-- Name: idx_16443_organization_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16443_organization_id ON atlas_transporter.inventory USING btree (organization_id);


--
-- Name: idx_16443_product_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16443_product_id ON atlas_transporter.inventory USING btree (product_id);


--
-- Name: idx_16402_city; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16402_city ON atlas_transporter.location USING btree (city);


--
-- Name: idx_16402_state; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16402_state ON atlas_transporter.location USING btree (state);


--
-- Name: idx_16435_entity_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16435_entity_id ON atlas_transporter.registration_token USING btree (entity_id);


--
-- Name: idx_16435_entity_type; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16435_entity_type ON atlas_transporter.registration_token USING btree (entity_type);

--
-- Name: idx_16451_organization_id; Type: INDEX; Schema: atlas_transporter; Owner: atlas
--

CREATE INDEX idx_16451_organization_id ON atlas_transporter.vehicle USING btree (organization_id);


CREATE INDEX idx_organization_short_id ON atlas_transporter.organization USING btree (short_id);

CREATE TABLE atlas_transporter.driver_stats (
    driver_id character(36) PRIMARY KEY NOT NULL REFERENCES atlas_transporter.person (id) on delete cascade,
    idle_since timestamp with time zone
);

CREATE TABLE atlas_transporter.transporter_config (
    id character(36) PRIMARY KEY NOT NULL,
    transporter_id character(36) NOT NULL,
    key character(36) NOT NULL,
    value character varying(1024) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    UNIQUE (transporter_id, key)
);

CREATE TABLE atlas_transporter.driver_information (
    driver_id character(36) PRIMARY KEY NOT NULL REFERENCES atlas_transporter.person (id) on delete cascade,
    active boolean DEFAULT false NOT NULL,
    on_ride boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_transporter.fare_policy (
    id character(36) NOT NULL,
    vehicle_variant character varying(255) NOT NULL,
    organization_id character varying(255) NOT NULL,
    base_fare double precision,
    base_distance double precision,
    per_extra_km_rate double precision NOT NULL,
    night_shift_start time,
    night_shift_end time,
    night_shift_rate double precision,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_transporter.fare_policy OWNER TO atlas;

CREATE TABLE atlas_transporter.ride_request (
    id character(36) PRIMARY KEY NOT NULL,
    ride_id character(36) NOT NULL REFERENCES atlas_transporter.product_instance (id) on delete cascade,
    short_org_id character varying(255) NOT NULL,
    created_at timestamp with time zone NOT NULL,
    type character varying(20) NOT NULL
);

CREATE TABLE atlas_transporter.notification_status (
    id character(36) PRIMARY KEY NOT NULL,
    ride_id character(36) NOT NULL REFERENCES atlas_transporter.product_instance (id) on delete cascade,
    driver_id character(36) NOT NULL REFERENCES atlas_transporter.person (id) on delete cascade,
    status character varying(20) NOT NULL,
    expires_at timestamp with time zone NOT NULL
);

CREATE TABLE atlas_transporter.allocation_event (
    id character(36) PRIMARY KEY NOT NULL,
    ride_id character(36) NOT NULL,
    driver_id character(36),
    event_type character varying(22) NOT NULL,
    timestamp timestamp with time zone NOT NULL
);
