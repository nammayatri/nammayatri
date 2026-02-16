-- Backfill existing queries with CLICKHOUSE
UPDATE atlas_driver_offer_bpp.chakra_queries SET query_type = 'CLICKHOUSE' WHERE query_type IS NULL;
