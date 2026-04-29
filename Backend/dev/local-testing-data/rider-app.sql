-- =============================================================================
-- Rider-app local-testing seed (atlas_app)
--
-- Two sections:
--   1. GENERIC (merchant-level, all-cities)
--      Per merchant_operating_city of BRIDGE_FINLAND, BRIDGE_CABS:
--        person + registration_token
--   2. INTERNATIONAL (city-level, BRIDGE_FINLAND only)
--      Sets +358 country code on the test rider so checkout-time validation
--      against international phone formats matches reality.
--
-- Adds NO new merchants and NO config rows. Merchants and operating cities
-- are expected to be present (seeded via config-sync prod import).
-- =============================================================================

-- ─────────────────────────────────────────────────────────────────────────────
-- SECTION 1: GENERIC — one test rider per merchant_operating_city
-- ─────────────────────────────────────────────────────────────────────────────
DO $$
DECLARE
    sample_enc CONSTANT TEXT  := '0.1.0|0|j4VGXmuvt7HU5qP+En30NqGAn2z8KpRGFeJVd8H/VAWPB3mliuNjeQWteRoXUC7yYOOMjV6m0uYkJqy/Ww==';
    sample_hash CONSTANT bytea := '\x1604f4cfb781d8f1c9ff024f8d06abc265a2d6954ac59079ea0c11f9f0bc1fa4';

    rec RECORD;
    city_idx INT := 0;
    rdr_id TEXT;
    country_code TEXT;
BEGIN
    TRUNCATE
        atlas_app.registration_token,
        atlas_app.person
    CASCADE;

    FOR rec IN
        SELECT moc.id, moc.merchant_id, moc.city, m.short_id
        FROM atlas_app.merchant_operating_city moc
        JOIN atlas_app.merchant m ON m.id = moc.merchant_id
        WHERE m.short_id IN ('BRIDGE_FINLAND', 'BRIDGE_CABS')
        ORDER BY m.short_id, moc.city, moc.id
    LOOP
        city_idx := city_idx + 1;
        rdr_id := 'test-rdr-' || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 23);
        country_code := CASE WHEN rec.short_id = 'BRIDGE_FINLAND' THEN '+358' ELSE '+91' END;

        INSERT INTO atlas_app.person (
            id, first_name, role, gender, identifier_type,
            mobile_number_encrypted, mobile_number_hash, mobile_country_code,
            merchant_id, merchant_operating_city_id,
            is_new, enabled, blocked, has_taken_valid_ride, is_valid_rating,
            created_at, updated_at
        ) VALUES (
            rdr_id, 'TestRider_' || rec.city, 'USER', 'UNKNOWN', 'MOBILENUMBER',
            sample_enc, sample_hash, country_code,
            rec.merchant_id, rec.id,
            false, true, false, false, true,
            now(), now()
        );

        INSERT INTO atlas_app.registration_token (
            id, auth_medium, auth_type, auth_value_hash, token,
            verified, auth_expiry, token_expiry, attempts,
            entity_id, entity_type, merchant_id,
            created_at, updated_at
        ) VALUES (
            'test-tok-' || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 23),
            'SMS', 'OTP', '9999',
            rdr_id,
            true, 3, 365, 3,
            rdr_id, 'USER',
            rec.merchant_id,
            now(), now()
        );
    END LOOP;

    RAISE NOTICE '[generic] Seeded % test rider(s) across BRIDGE_FINLAND / BRIDGE_CABS cities', city_idx;
END $$;


-- ─────────────────────────────────────────────────────────────────────────────
-- SECTION 2: INTERNATIONAL — extras for BRIDGE_FINLAND cities
-- The country_code is already +358 from Section 1, but this section is
-- where any future Stripe-payment-method seeding for the rider side belongs
-- (e.g. a saved card on file, default payment method preference, etc.).
-- ─────────────────────────────────────────────────────────────────────────────
DO $$
DECLARE
    rec RECORD;
    city_idx INT := 0;
BEGIN
    FOR rec IN
        SELECT p.id AS rider_id
        FROM atlas_app.person p
        JOIN atlas_app.merchant m ON m.id = p.merchant_id
        WHERE m.short_id = 'BRIDGE_FINLAND' AND p.role = 'USER'
    LOOP
        city_idx := city_idx + 1;
        -- Bump the rider's language to ensure UI strings resolve correctly
        -- in international flows (placeholder — replace with actual
        -- payment-method seeding when those rider-side tables are wired up).
        UPDATE atlas_app.person
           SET language = 'ENGLISH', updated_at = now()
         WHERE id = rec.rider_id;
    END LOOP;

    RAISE NOTICE '[international] Updated % BRIDGE_FINLAND test rider(s) for international flow', city_idx;
END $$;
