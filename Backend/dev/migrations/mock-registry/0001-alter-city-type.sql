ALTER TABLE atlas_registry.subscriber ALTER COLUMN city type text[] USING (ARRAY[city])::text[];
