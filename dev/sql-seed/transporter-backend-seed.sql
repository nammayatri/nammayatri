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
    price numeric(30,10) NOT NULL,
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
    identifier_type character varying(255),
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
-- Data for Name: organization; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

-- Gateway entries
INSERT INTO atlas_transporter.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, created_at, updated_at, info) VALUES
  ('1926d40f-1223-4eb2-ba5d-7983bde2fd02', 'Juspay Gateway', 'JUSPAY.BG.1', NULL, 'APPROVED', 'GATEWAY', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'juspay-gateway-bpp-key', 'http://localhost:8015/v1', 'mobility-provider-key', NULL, '2020-06-08 18:37:00+00', '2020-06-08 18:37:00+00', NULL);
-- NSDL creds are fetched from config
INSERT INTO atlas_transporter.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, created_at, updated_at, info) VALUES
  ('0d7016d0-f9cd-4f9f-886f-bc4cbd6a86e5', 'NSDL Gateway', 'NSDL', NULL, 'APPROVED', 'GATEWAY', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'nsdl-gateway-bpp-key', NULL, NULL, NULL, '2020-06-08 18:37:00+00', '2020-06-08 18:37:00+00', NULL);

-- Transporter entries. api_key, callback_url, callback_api_key are not needed to be populated. These are only looked up from id.
INSERT INTO atlas_transporter.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, created_at, updated_at, info) VALUES
  ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '[A] Transporter #1', 'JUSPAY.MOBILITY.PROVIDER.UAT.1', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', false, true, 'e95d2f36-a455-4625-bfb4-22807fefa1eb', NULL, '9888888888', '+91', NULL, NULL, NULL, NULL, NULL, NULL, '2020-07-28 16:05:57.92753+00', '2020-07-28 16:05:57.92753+00', NULL);
INSERT INTO atlas_transporter.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, created_at, updated_at, info) VALUES
  ('e1f37274-f0aa-4bb3-93a0-2476349487b7', 'Another Test Cabs', 'another-test-cabs', NULL, 'APPROVED', 'PROVIDER', 'MOBILITY', true, true, 'e95d2f36-a455-4625-bfb4-22807fefa1eb', NULL, '9777777777', '+91', NULL, NULL, NULL, NULL, NULL, NULL, '2020-07-28 16:05:57.92753+00', '2020-07-28 16:05:57.92753+00', NULL);

-- BAP app entry.
INSERT INTO atlas_transporter.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, created_at, updated_at, info) VALUES
  ('505e4651-5340-4836-81a7-045394ba6dc3', 'Mobility BAP', 'JUSPAY.MOBILITY.APP.UAT.1', NULL, 'APPROVED', 'APP', 'MOBILITY', true, true, NULL, NULL, '9777777777', '+91', NULL, NULL, 'bpp-1-key', 'http://localhost:8013/v1', 'bap-key-1', NULL, '2020-07-28 16:05:57.92753+00', '2020-07-28 16:05:57.92753+00', NULL);

-- NSDL entry
INSERT INTO atlas_transporter.organization (id, name, short_id, gstin, status, type, domain, verified, enabled, location_id, description, mobile_number, mobile_country_code, from_time, to_time, api_key, callback_url, callback_api_key, head_count, info, created_at, updated_at) VALUES
  ('239ee68b-0818-4dba-ad31-032fe809cf71', 'NSDL Gateway', 'NSDL.BG.1', NULL, 'APPROVED', 'GATEWAY', 'MOBILITY', true, true, NULL, NULL, NULL, NULL, NULL, NULL, 'nsdl-gateway-key', 'https:// pilot-gateway-1.beckn.nsdl.co.in/v1', 'nsdl-app-key', NULL, NULL, '2020-09-28 16:05:57.92753+00', '2020-09-28 16:05:57.92753+00');

--
-- Data for Name: person; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

INSERT INTO atlas_transporter.person (id, first_name, middle_name, last_name, full_name, role, gender, identifier_type, email, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, rating, verified, udf1, udf2, status, organization_id, device_token, location_id, description, created_at, updated_at) VALUES
  ('6bc4bc84-2c43-425d-8853-22f47bd06691', 'Suresh', 'aka', 'Dhinesh', NULL, 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, '0.1.0|0|iP3CepsEe8Qmw1xbLR5HJFSESfdvU2tWtNWrdCZWtwp4msTfh1BDkc95/yytpllMp61Q8mpiS+KDde+Plw==', '\xa0a56e902b973e6cf231520c2acbda9b44947dd3a88fb0daacd23d68082c6362', '+91', NULL, NULL, false, '0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE', 'INACTIVE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, '9db237f9-3061-4bad-a616-42e91f092407', NULL, '2020-07-28 16:06:47.042159+00', '2020-07-28 16:06:47.042159+00');
INSERT INTO atlas_transporter.person (id, first_name, middle_name, last_name, full_name, role, gender, identifier_type, email, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, rating, verified, udf1, udf2, status, organization_id, device_token, location_id, description, created_at, updated_at) VALUES
  ('3b2af603-c6a3-404e-9859-276085fc6e65', 'Thomas', 'aka', NULL, NULL, 'DRIVER', 'MALE', 'MOBILENUMBER', NULL, NULL, NULL, '+91', NULL, NULL, false, 'f1b84838-5cb5-4eb1-8f4b-0dd0c4a2943b', 'VEHICLE', 'INACTIVE', 'e1f37274-f0aa-4bb3-93a0-2476349487b7', NULL, '0202b94d-dbf0-4e95-bbf9-25cafc888173', NULL, '2020-07-28 16:06:47.042159+00', '2020-07-28 16:06:47.042159+00');
INSERT INTO atlas_transporter.person (id, first_name, middle_name, last_name, full_name, role, gender, identifier_type, email, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, rating, verified, udf1, udf2, status, organization_id, device_token, location_id, description, created_at, updated_at) VALUES
  ('ec34eede-5a3e-4a41-89d4-7290a0d7a629', NULL, NULL, NULL, NULL, 'ADMIN', 'UNKNOWN', 'MOBILENUMBER', NULL, '0.1.0|2|eLbi245mKsDG3RKb3t2ah1VjwVUEWb/czljklq+ZaRU9PvRUfoYXODW7h6lexchLSjCS4DW31iDFqhYjCUw8Tw==', '\x0f298b3402584898975230a0a6c71362eab1bb7fbb4df662c1ce9f9ea8d08426', '91', '+919999999999', NULL, true, NULL, NULL, 'INACTIVE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', NULL, NULL, NULL, '2020-06-08 18:37:00+00', '2020-06-08 18:37:00+00');
INSERT INTO atlas_transporter.person (id, first_name, middle_name, last_name, full_name, role, gender, identifier_type, email, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, rating, verified, udf1, udf2, status, organization_id, device_token, location_id, description, created_at, updated_at) VALUES
  ('a30193df-4f7c-440f-bada-4d46c396d7d0', NULL, NULL, NULL, NULL, 'ADMIN', 'UNKNOWN', 'MOBILENUMBER', NULL, NULL, NULL, '91', '9999988888', NULL, true, NULL, NULL, 'INACTIVE', 'e1f37274-f0aa-4bb3-93a0-2476349487b7', NULL, NULL, NULL, '2020-06-08 18:37:00+00', '2020-06-08 18:37:00+00');

INSERT INTO atlas_transporter.person (id, role, gender, verified, status, organization_id, location_id, udf1, udf2) VALUES
  ('furthest_driver', 'DRIVER', 'MALE',   true, 'ACTIVE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'furthest_driver', '0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE'),
  ('closest_driver',  'DRIVER', 'MALE',   true, 'ACTIVE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'closest_driver',  '0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE'),
  ('other_driver',    'DRIVER', 'FEMALE', true, 'ACTIVE', 'e1f37274-f0aa-4bb3-93a0-2476349487b7', 'other_driver',    '0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', 'VEHICLE');

--
-- Data for Name: product; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

INSERT INTO atlas_transporter.product (id, name, industry, type, status, short_id, price, created_at, updated_at) values
  ('998af371-e726-422e-8356-d24085a1d586', 'AUTO', 'MOBILITY', 'RIDE', 'INSTOCK', 'Dney75jyIwsKoR7a', '0.0000000000', '2020-07-08 16:49:38.726748+00', '2020-07-08 16:49:38.726748+00');
INSERT INTO atlas_transporter.product (id, name, industry, type, status, short_id, price, created_at, updated_at) values
  ('5ad086dd-c5c1-49a6-b66d-245d13b70194', 'HATCHBACK', 'MOBILITY', 'RIDE', 'INSTOCK', 'EBL2GZPJAHvaxLRO', '0.0000000000', '2020-07-08 16:50:01.263198+00', '2020-07-08 16:50:01.263198+00');
INSERT INTO atlas_transporter.product (id, name, industry, type, status, short_id, price, created_at, updated_at) values
  ('f726d2fa-2df1-42f0-a009-6795cfdc9b05', 'SUV', 'MOBILITY', 'RIDE', 'INSTOCK', 'SldbUk7Kplnz7B6X', '0.0000000000', '2020-07-08 16:50:05.31276+00', '2020-07-08 16:50:05.31276+00');
INSERT INTO atlas_transporter.product (id, name, industry, type, status, short_id, price, created_at, updated_at) values
  ('ad044fd7-2b62-4f37-93da-e48fe0678de1', 'SEDAN', 'MOBILITY', 'RIDE', 'INSTOCK', 'UINnHxAgoQlqkRrD', '0.0000000000', '2020-07-08 16:50:09.231255+00', '2020-07-08 16:50:09.231255+00');

--
-- Data for Name: registration_token; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

INSERT INTO atlas_transporter.registration_token (id, auth_medium, auth_type, auth_value_hash, token, verified, auth_expiry, token_expiry, attempts, entity_id, entity_type, created_at, updated_at) values
  ('772453e2-d02b-494a-a4ac-ec1ea0027e18', 'SMS', 'OTP', '3249', 'ea37f941-427a-4085-a7d0-96240f166672', true, 3, 365, 3, 'ec34eede-5a3e-4a41-89d4-7290a0d7a629', 'USER', '2020-06-08 18:37:00+00', '2020-06-08 18:37:00+00');

INSERT INTO atlas_transporter.registration_token (id, auth_medium, auth_type, auth_value_hash, token, verified, auth_expiry, token_expiry, attempts, entity_id, entity_type, created_at, updated_at) values
  ('c38562c2-3d58-4b08-8496-1f33b8352fe0', 'SMS', 'OTP', '3249', 'ca05cf3c-c88b-4a2f-8874-191659397e0d', true, 3, 365, 3, '6bc4bc84-2c43-425d-8853-22f47bd06691', 'USER', now(), now());

--
-- Data for Name: vehicle; Type: TABLE DATA; Schema: atlas_transporter; Owner: atlas
--

INSERT INTO atlas_transporter.vehicle (id, capacity, category, make, model, size, variant, color, energy_type, registration_no, registration_category, organization_id, created_at, updated_at) VALUES
  ('0c1cd0bc-b3a4-4c6c-811f-900ccf4dfb94', NULL, NULL, NULL, NULL, NULL, 'SUV', 'Black', NULL, '4810', NULL, '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '2020-07-28 16:07:04.203777+00', '2020-07-28 16:07:04.203777+00');

INSERT INTO atlas_transporter.vehicle (id, capacity, category, make, model, size, variant, color, energy_type, registration_no, registration_category, organization_id, created_at, updated_at) VALUES
  ('f1b84838-5cb5-4eb1-8f4b-0dd0c4a2943b', NULL, NULL, NULL, NULL, NULL, 'SUV', 'Black', NULL, '5613', NULL, 'e1f37274-f0aa-4bb3-93a0-2476349487b7', '2020-07-28 16:07:04.203777+00', '2020-07-28 16:07:04.203777+00');

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
  ADD CONSTRAINT unique_mobile_number_country_code UNIQUE (mobile_country_code, mobile_number_hash);

ALTER TABLE ONLY atlas_transporter.person
  ADD CONSTRAINT unique_identifier UNIQUE (identifier);

ALTER TABLE ONLY atlas_transporter.person
  ADD CONSTRAINT unique_email UNIQUE (email);

ALTER TABLE ONLY atlas_transporter.organization
  ADD CONSTRAINT unique_api_key UNIQUE (api_key);

ALTER TABLE ONLY atlas_transporter.organization
  ADD CONSTRAINT unique_short_id UNIQUE (short_id);

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


UPDATE atlas_transporter.person SET
    mobile_number_encrypted = '0.1.0|2|eLbi245mKsDG3RKb3t2ah1VjwVUEWb/czljklq+ZaRU9PvRUfoYXODW7h6lexchLSjCS4DW31iDFqhYjCUw8Tw=='
  , mobile_number_hash = decode('0f298b3402584898975230a0a6c71362eab1bb7fbb4df662c1ce9f9ea8d08426', 'hex') where id = 'ec34eede-5a3e-4a41-89d4-7290a0d7a629';

--
-- PostgreSQL database dump complete
--


CREATE TABLE atlas_transporter.trail (
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

ALTER TABLE ONLY atlas_transporter."trail"
    ADD CONSTRAINT idx_trail_primary PRIMARY KEY (id);



CREATE TABLE atlas_transporter.external_trail (
    id character(36) NOT NULL,
    --customer_id character(36),
    --session_id character(36),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    gateway_id character varying(36) NOT NULL,
    endpoint_id character varying(36) NOT NULL,
    headers text NOT NULL,
    query_params text NOT NULL,
    request text,  -- TODO: do we want to limit size of request somehow?
    succeeded boolean,
    response text,  -- TODO: do we want to limit size of response somehow?
                    -- reponse usually contains a list, so data can be infinitely large
    error text
);

ALTER TABLE ONLY atlas_transporter."external_trail"
    ADD CONSTRAINT idx_external_trail_primary PRIMARY KEY (id);

INSERT INTO atlas_transporter.location (id, location_type, lat, long) VALUES
  ('e95d2f36-a455-4625-bfb4-22807fefa1eb', 'POINT', 10.082713, 76.268572);

INSERT INTO atlas_transporter.location (id, location_type, point) VALUES
                                                              -- lon        lat
  ('furthest_driver', 'POINT', public.ST_SetSRID(public.ST_Point(77.593360, 13.005432), 4326)),
  ('closest_driver',  'POINT', public.ST_SetSRID(public.ST_Point(77.601921, 12.995477), 4326)),
  ('other_driver',    'POINT', public.ST_SetSRID(public.ST_Point(77.601922, 12.995478), 4326));

INSERT INTO atlas_transporter.location (id, location_type, long, lat, point) VALUES
  ('0202b94d-dbf0-4e95-bbf9-25cafc888173', 'POINT', 76.2733, 10.0739, public.ST_SetSRID(public.ST_Point(76.2733, 10.0739), 4326));
INSERT INTO atlas_transporter.location (id, location_type, long, lat, point) VALUES
  ('9db237f9-3061-4bad-a616-42e91f092407', 'POINT', 76.2733, 10.0739, public.ST_SetSRID(public.ST_Point(76.2733, 10.0739), 4326));

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

INSERT INTO atlas_transporter.driver_information (driver_id, active, on_ride, created_at, updated_at) select id, True, False, now(), now() from atlas_transporter.person where role ='DRIVER';
INSERT INTO atlas_transporter.driver_stats (driver_id, idle_since) SELECT id, now() FROM atlas_transporter.person WHERE role ='DRIVER';

ALTER TABLE atlas_transporter.fare_policy OWNER TO atlas;

INSERT INTO atlas_transporter.fare_policy (id, vehicle_variant, organization_id, base_fare, base_distance, per_extra_km_rate, night_shift_start, night_shift_end, night_shift_rate) VALUES
  ('9a978cc8-584b-4bd1-ad7f-0c061e25b92d', 'SUV', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 120.0, 5000.0, 12.0, '21:00:00', '5:00:00', 1.1);

INSERT INTO atlas_transporter.fare_policy (id, vehicle_variant, organization_id, base_fare, base_distance, per_extra_km_rate, night_shift_start, night_shift_end, night_shift_rate) VALUES
  ('f777fbfa-53a7-4f68-90b8-b55492f01b8b', 'SUV', 'e1f37274-f0aa-4bb3-93a0-2476349487b7', 120.0, 5000.0, 12.0, '21:00:00', '5:00:00', 1.1);

CREATE TABLE atlas_transporter.ride_request (
    id character(36) PRIMARY KEY NOT NULL,
    ride_id character(36) NOT NULL REFERENCES atlas_transporter.product_instance (id) on delete cascade,
    created_at timestamp with time zone,
    last_process_time timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    type character varying(20) NOT NULL,
    status character varying(20) NOT NULL
);

CREATE TABLE atlas_transporter.notification_status (
    id character(36) PRIMARY KEY NOT NULL,
    ride_id character(36) NOT NULL REFERENCES atlas_transporter.product_instance (id) on delete cascade,
    driver_id character(36) NOT NULL REFERENCES atlas_transporter.person (id) on delete cascade,
    status character varying(20) NOT NULL,
    notified_at timestamp with time zone
);
