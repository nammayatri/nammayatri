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

CREATE SCHEMA atlas_scheduler_example;

ALTER SCHEMA atlas_scheduler_example OWNER TO atlas;

SET default_tablespace = '';

SET default_table_access_method = heap;

CREATE USER atlas_scheduler_example_user WITH PASSWORD 'atlas';
ALTER SCHEMA atlas_scheduler_example OWNER TO atlas_scheduler_example_user;


