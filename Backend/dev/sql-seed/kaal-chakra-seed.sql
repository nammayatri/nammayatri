CREATE USER kaal_chakra_user WITH PASSWORD 'atlas';
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
CREATE SCHEMA kaal_chakra;
ALTER SCHEMA kaal_chakra OWNER TO kaal_chakra_user;

CREATE TABLE kaal_chakra.scheduler_job (
  id character varying(255) PRIMARY KEY,
  job_type character varying(255) NOT NULL,
  job_data text NOT NULL,
  shard_id int NOT NULL,
  scheduled_at timestamp NOT NULL,
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL,
  max_errors int NOT NULL,
  curr_errors int NOT NULL,
  status character varying(255) NOT NULL,
  parent_job_id character(36) NOT NULL
);

ALTER TABLE kaal_chakra.scheduler_job OWNER TO kaal_chakra_user;
