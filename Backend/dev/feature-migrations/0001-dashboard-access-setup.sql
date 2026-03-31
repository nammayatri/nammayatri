-- Dashboard access for local testing
-- Inserts operating cities + admin merchant_access rows for both BPP and BAP dashboards.

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
    -- BPP dashboard merchant_access
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

    -- BAP dashboard merchant_access
    FOR r IN SELECT id, short_id FROM atlas_bap_dashboard.merchant
    LOOP
        FOREACH city_name IN ARRAY ARRAY['Bangalore','Helsinki','Kolkata','Chennai','Delhi']
        LOOP
            INSERT INTO atlas_bap_dashboard.merchant_access (id, person_id, merchant_id, merchant_short_id, operating_city, secret_key, is2fa_enabled, created_at)
            SELECT gen_random_uuid()::text, admin_person_id, r.id, r.short_id, city_name, '', false, now()
            WHERE NOT EXISTS (
                SELECT 1 FROM atlas_bap_dashboard.merchant_access
                WHERE person_id = admin_person_id AND operating_city = city_name AND merchant_id = r.id
            );
        END LOOP;
    END LOOP;

    -- BAP registration token for testing
    INSERT INTO atlas_bap_dashboard.registration_token (id, token, person_id, created_at, merchant_id, operating_city, enabled)
    SELECT gen_random_uuid()::text, 'bap-admin-token-for-testing', admin_person_id, now(),
           (SELECT id FROM atlas_bap_dashboard.merchant LIMIT 1), 'Bangalore', true
    WHERE NOT EXISTS (
        SELECT 1 FROM atlas_bap_dashboard.registration_token WHERE token = 'bap-admin-token-for-testing'
    );
END $$;
