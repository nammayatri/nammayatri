-- Add query_type column to chakra_queries table (nullable for backwards compatibility)
ALTER TABLE atlas_app.chakra_queries ADD COLUMN query_type text;

-- Backfill existing queries with CLICKHOUSE
UPDATE atlas_app.chakra_queries SET query_type = 'CLICKHOUSE' WHERE query_type IS NULL;
