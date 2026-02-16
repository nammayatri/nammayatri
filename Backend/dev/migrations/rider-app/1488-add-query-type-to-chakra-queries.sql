-- Backfill existing queries with CLICKHOUSE
UPDATE atlas_app.chakra_queries SET query_type = 'CLICKHOUSE' WHERE query_type IS NULL;
