-- Superuser for config-sync (can SET session_replication_role to disable FK checks during bulk import)
DO $$ BEGIN
  IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'atlas_superuser') THEN
    CREATE ROLE atlas_superuser WITH LOGIN SUPERUSER PASSWORD 'atlas';
  END IF;
END $$;

-- Enable postgis extension
-- Got following queries from https://github.com/postgis/docker-postgis/blob/master/initdb-postgis.sh#L17-L23
CREATE EXTENSION IF NOT EXISTS postgis;
CREATE EXTENSION IF NOT EXISTS postgis_topology;
-- Reconnect to update pg_setting.resetval
-- See https://github.com/postgis/docker-postgis/issues/288
\c
CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;
CREATE EXTENSION IF NOT EXISTS postgis_tiger_geocoder;