-- Enable postgis extension
-- Got following queries from https://github.com/postgis/docker-postgis/blob/master/initdb-postgis.sh#L17-L23
CREATE EXTENSION IF NOT EXISTS postgis;
CREATE EXTENSION IF NOT EXISTS postgis_topology;
-- Reconnect to update pg_setting.resetval
-- See https://github.com/postgis/docker-postgis/issues/288
\c
CREATE EXTENSION IF NOT EXISTS fuzzystrmatch;
CREATE EXTENSION IF NOT EXISTS postgis_tiger_geocoder;