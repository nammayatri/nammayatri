-- Backfill NULL ride_id values with generated UUIDs for non-ride SOS records
UPDATE atlas_app.sos
SET ride_id = gen_random_uuid()::text
WHERE ride_id IS NULL;