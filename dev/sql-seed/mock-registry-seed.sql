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
-- Name: atlas_registry; Type: SCHEMA; Schema: -; Owner: atlas
--

CREATE SCHEMA atlas_registry;

ALTER SCHEMA atlas_registry OWNER TO atlas;


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: subscriber; Type: TABLE; Schema: atlas_registry; Owner: atlas
--

CREATE TABLE atlas_registry.subscriber (
    unique_key_id character varying(255) NOT NULL,
    subscriber_id character varying(255) NOT NULL,
    subscriber_url character varying(255) NOT NULL,
    type character varying(255) NOT NULL,
    domain character varying(255) NOT NULL,
    city character varying(255),
    country character varying(255),
    status character varying(255),
    signing_public_key character varying(255) NOT NULL,
    encr_public_key character varying(255),
    valid_from timestamp with time zone,
    valid_until timestamp with time zone,
    created timestamp with time zone DEFAULT now() NOT NULL,
    updated timestamp with time zone DEFAULT now() NOT NULL,
    PRIMARY KEY (unique_key_id, subscriber_id)
);


ALTER TABLE atlas_registry.subscriber OWNER TO atlas;