-- Post-import setup: Dashboard access for local testing
-- Run after config-sync import (auto-runs from cmd_import, or manually):
--   psql -h localhost -p 5434 -U atlas_superuser -d atlas_dev -f post-import-setup.sql
--
-- Most patches (signing keys, service URLs, OSRM, subscriber fixes, integrated_bpp_config)
-- are now in patches.json and applied automatically during import.
-- This file only contains operations that patches.json can't express:
--   - INSERT new rows (dashboard operating cities, merchant_access)

-- ══════════════════════════════════════════════════════════════
-- 1. Dashboard operating cities (INSERT IF NOT EXISTS)
-- ══════════════════════════════════════════════════════════════

INSERT INTO atlas_bpp_dashboard.merchant_operating_city (id, city)
SELECT gen_random_uuid()::text, c.city
FROM (VALUES ('Bangalore'), ('Helsinki'), ('Kolkata'), ('Chennai'), ('Delhi')) AS c(city)
WHERE NOT EXISTS (SELECT 1 FROM atlas_bpp_dashboard.merchant_operating_city WHERE city = c.city);

INSERT INTO atlas_bap_dashboard.merchant_operating_city (id, city)
SELECT gen_random_uuid()::text, c.city
FROM (VALUES ('Bangalore'), ('Helsinki'), ('Kolkata'), ('Chennai'), ('Delhi')) AS c(city)
WHERE NOT EXISTS (SELECT 1 FROM atlas_bap_dashboard.merchant_operating_city WHERE city = c.city);

-- ══════════════════════════════════════════════════════════════
-- 2. Dashboard admin merchant_access for all cities
--    Enables switchMerchantAndCity API for the admin token
-- ══════════════════════════════════════════════════════════════

DO $$
DECLARE
    admin_person_id TEXT := '3680f4b5-dce4-4d03-aa8c-5405690e87bd';
    r RECORD;
    city_name TEXT;
BEGIN
    FOR r IN SELECT id, short_id FROM atlas_bpp_dashboard.merchant
    LOOP
        FOREACH city_name IN ARRAY ARRAY['Bangalore','Helsinki','Kolkata','Chennai','Delhi']
        LOOP
            INSERT INTO atlas_bpp_dashboard.merchant_access (id, person_id, merchant_id, merchant_short_id, operating_city, secret_key, is2fa_enabled, created_at)
            SELECT gen_random_uuid()::text, admin_person_id, r.id, r.short_id, city_name, '', false, now()
            WHERE NOT EXISTS (
                SELECT 1 FROM atlas_bpp_dashboard.merchant_access
                WHERE person_id = admin_person_id AND operating_city = city_name AND merchant_id = r.id
            );
        END LOOP;
    END LOOP;
END $$;
