-- =============================================================================
-- Driver-app local-testing seed (atlas_driver_offer_bpp)
--
-- Three sections, each self-contained (own person + supporting rows):
--   1. GENERIC          — BRIDGE_CABS_PARTNER cities (domestic, default flow)
--   2. INTERNATIONAL    — BRIDGE_FINLAND_PARTNER cities (+358, EUR/Stripe)
--   3. FLEET            — MSIL_PARTNER cities (driver + fleet_owner + RC link)
--
-- Sections do not share rows. Section 1 TRUNCATEs all transactional tables;
-- sections 2 and 3 INSERT their own person / driver_information / vehicle /
-- driver_stats / driver_license / vehicle_registration_certificate /
-- driver_rc_association / registration_token rows for their merchants.
--
-- Adds NO new merchants and NO config rows. Merchants and operating cities
-- are expected to be present (seeded via config-sync prod import).
--
-- Encrypted mobile / document blobs are passetto-encrypted against the local
-- test passetto key, sourced from the dashboard local-testing seed.
-- =============================================================================

-- ─────────────────────────────────────────────────────────────────────────────
-- SECTION 1: GENERIC — one test driver per BRIDGE_CABS_PARTNER city
-- (BRIDGE_FINLAND_PARTNER handled in SECTION 2, MSIL_PARTNER in SECTION 3.)
-- ─────────────────────────────────────────────────────────────────────────────
DO $$
DECLARE
    sample_enc CONSTANT TEXT  := '0.1.0|0|j4VGXmuvt7HU5qP+En30NqGAn2z8KpRGFeJVd8H/VAWPB3mliuNjeQWteRoXUC7yYOOMjV6m0uYkJqy/Ww==';
    sample_hash CONSTANT bytea := '\x1604f4cfb781d8f1c9ff024f8d06abc265a2d6954ac59079ea0c11f9f0bc1fa4';
    sample_doc_enc CONSTANT TEXT := '0.1.0|0|j4VGXmuvt7HU5qP+En30NqGAn2z8KpRGFeJVd8H/VAWPB3mliuNjeQWteRoXUC7yYOOMjV6m0uYkJqy/Ww==';

    rec RECORD;
    city_idx INT := 0;
    drv_id TEXT;
    veh_no TEXT;
    rc_id TEXT;
    dl_id TEXT;
    rc_assoc_id TEXT;
    dl_hash bytea;
    rc_hash bytea;
BEGIN
    -- Wipe transactional test tables. CASCADE so dependent rows in
    -- search_request, ride, etc. don't block the truncate.
    TRUNCATE
        atlas_driver_offer_bpp.driver_rc_association,
        atlas_driver_offer_bpp.vehicle_registration_certificate,
        atlas_driver_offer_bpp.driver_license,
        atlas_driver_offer_bpp.driver_stats,
        atlas_driver_offer_bpp.vehicle,
        atlas_driver_offer_bpp.driver_information,
        atlas_driver_offer_bpp.registration_token,
        atlas_driver_offer_bpp.person
    CASCADE;

    FOR rec IN
        SELECT moc.id, moc.merchant_id, moc.city
        FROM atlas_driver_offer_bpp.merchant_operating_city moc
        JOIN atlas_driver_offer_bpp.merchant m ON m.id = moc.merchant_id
        WHERE m.short_id = 'BRIDGE_CABS_PARTNER'
        ORDER BY moc.city, moc.id
    LOOP
        city_idx := city_idx + 1;
        drv_id      := 'test-drv-'   || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 23);
        veh_no      := 'TEST-'       || lpad(city_idx::text, 6, '0');
        dl_id       := 'test-dl-'    || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 24);
        rc_id       := 'test-rc-'    || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 24);
        rc_assoc_id := 'test-rcas-'  || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 22);
        dl_hash := sha256(('test-dl-' || city_idx)::bytea);
        rc_hash := sha256(('test-rc-' || city_idx)::bytea);

        INSERT INTO atlas_driver_offer_bpp.person (
            id, first_name, last_name, role, gender, identifier_type,
            mobile_number_encrypted, mobile_number_hash, mobile_country_code,
            merchant_id, merchant_operating_city_id,
            onboarded_from_dashboard, is_new, total_earned_coins, used_coins, language,
            created_at, updated_at
        ) VALUES (
            drv_id, 'TestDriver_' || rec.city, 'TestLast', 'DRIVER', 'MALE', 'MOBILENUMBER',
            sample_enc, sample_hash, '+91',
            rec.merchant_id, rec.id,
            false, false, 0, 0, 'ENGLISH',
            now(), now()
        );

        INSERT INTO atlas_driver_offer_bpp.driver_information (
            driver_id, active, enabled, verified, on_ride, blocked,
            num_of_locks, payment_pending, subscribed,
            can_downgrade_to_hatchback, can_downgrade_to_sedan, can_downgrade_to_taxi,
            aadhaar_verified, ac_restriction_lift_count,
            can_switch_to_intra_city, can_switch_to_inter_city, can_switch_to_rental,
            created_at, updated_at
        ) VALUES (
            drv_id, false, true, true, false, false,
            0, false, true,
            false, false, false,
            false, 0,
            true, true, true,
            now(), now()
        );

        INSERT INTO atlas_driver_offer_bpp.vehicle (
            driver_id, merchant_id, variant, model, color, registration_no,
            category, make, capacity, vehicle_class, selected_service_tiers,
            created_at, updated_at
        ) VALUES (
            drv_id, rec.merchant_id, 'SEDAN', 'Test Sedan', 'White', veh_no,
            'CAR', 'TestMake', 4, '3WT', '{SEDAN,COMFY}',
            now(), now()
        );

        INSERT INTO atlas_driver_offer_bpp.driver_stats (
            driver_id, bonus_earned, earnings_missed, idle_since,
            late_night_trips, total_distance, total_earnings, total_rides,
            fav_rider_count, updated_at
        ) VALUES (
            drv_id, 0, 0, now(),
            0, 0, 0, 0,
            0, now()
        );

        INSERT INTO atlas_driver_offer_bpp.driver_license (
            id, driver_id, merchant_id,
            consent, consent_timestamp,
            document_image_id1, document_image_id2,
            driver_dob, driver_name,
            license_expiry,
            license_number_hash, license_number_encrypted,
            verification_status, failed_rules,
            class_of_vehicles, date_of_issue, reject_reason, vehicle_category,
            created_at, updated_at
        ) VALUES (
            dl_id, drv_id, rec.merchant_id,
            true, now(),
            dl_id || '-img1', NULL,
            '1990-01-01 00:00:00+00', 'TestDriver_' || rec.city,
            '2099-12-31 00:00:00+00',
            dl_hash, sample_doc_enc,
            'VALID', '{}',
            '{"MCWG", "LMV", "3W-PV"}', '2020-01-01 00:00:00+00', NULL, 'CAR',
            now(), now()
        );

        INSERT INTO atlas_driver_offer_bpp.vehicle_registration_certificate (
            id, fleet_owner_id,
            certificate_number_hash, certificate_number_encrypted,
            unencrypted_certificate_number,
            fitness_expiry,
            created_at, verification_status, updated_at, failed_rules,
            document_image_id, vehicle_variant,
            merchant_id, merchant_operating_city_id
        ) VALUES (
            rc_id, NULL,
            rc_hash, sample_doc_enc,
            'TEST-RC-' || lpad(city_idx::text, 6, '0'),
            '2099-12-31 00:00:00+00',
            now(), 'VALID', now(), '{}',
            rc_id || '-img', 'SEDAN',
            rec.merchant_id, rec.id
        );

        INSERT INTO atlas_driver_offer_bpp.driver_rc_association (
            id, driver_id, rc_id,
            is_rc_active,
            associated_on, associated_till,
            consent, consent_timestamp,
            merchant_id, merchant_operating_city_id,
            error_message
        ) VALUES (
            rc_assoc_id, drv_id, rc_id,
            true,
            now(), '2099-12-31 00:00:00+00',
            true, now(),
            rec.merchant_id, rec.id,
            NULL
        );

        INSERT INTO atlas_driver_offer_bpp.registration_token (
            id, auth_medium, auth_type, auth_value_hash, token,
            verified, auth_expiry, token_expiry, attempts,
            entity_id, entity_type, merchant_id,
            created_at, updated_at
        ) VALUES (
            'test-tok-' || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 23),
            'SMS', 'OTP', '9999',
            drv_id,
            true, 3, 365, 3,
            drv_id, 'USER',
            rec.merchant_id,
            now(), now()
        );
    END LOOP;

    RAISE NOTICE '[generic] Seeded % test driver(s) across BRIDGE_CABS_PARTNER cities', city_idx;
END $$;


-- ─────────────────────────────────────────────────────────────────────────────
-- SECTION 2: INTERNATIONAL — BRIDGE_FINLAND_PARTNER cities
-- Self-contained: own person + driver_information + vehicle + stats +
-- driver_license + vehicle_registration_certificate + driver_rc_association +
-- registration_token. Country code +358, language ENGLISH.
-- ─────────────────────────────────────────────────────────────────────────────
DO $$
DECLARE
    sample_enc CONSTANT TEXT  := '0.1.0|0|j4VGXmuvt7HU5qP+En30NqGAn2z8KpRGFeJVd8H/VAWPB3mliuNjeQWteRoXUC7yYOOMjV6m0uYkJqy/Ww==';
    sample_hash CONSTANT bytea := '\x1604f4cfb781d8f1c9ff024f8d06abc265a2d6954ac59079ea0c11f9f0bc1fa4';
    sample_doc_enc CONSTANT TEXT := '0.1.0|0|j4VGXmuvt7HU5qP+En30NqGAn2z8KpRGFeJVd8H/VAWPB3mliuNjeQWteRoXUC7yYOOMjV6m0uYkJqy/Ww==';

    rec RECORD;
    city_idx INT := 0;
    drv_id TEXT;
    veh_no TEXT;
    rc_id TEXT;
    dl_id TEXT;
    rc_assoc_id TEXT;
    dl_hash bytea;
    rc_hash bytea;
BEGIN
    FOR rec IN
        SELECT moc.id, moc.merchant_id, moc.city
        FROM atlas_driver_offer_bpp.merchant_operating_city moc
        JOIN atlas_driver_offer_bpp.merchant m ON m.id = moc.merchant_id
        WHERE m.short_id = 'BRIDGE_FINLAND_PARTNER'
        ORDER BY moc.city, moc.id
    LOOP
        city_idx := city_idx + 1;
        drv_id      := 'test-intl-drv-'  || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 18);
        veh_no      := 'INTL-'           || lpad(city_idx::text, 6, '0');
        dl_id       := 'test-intl-dl-'   || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 19);
        rc_id       := 'test-intl-rc-'   || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 19);
        rc_assoc_id := 'test-intl-rcas-' || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 17);
        dl_hash := sha256(('test-intl-dl-' || city_idx)::bytea);
        rc_hash := sha256(('test-intl-rc-' || city_idx)::bytea);

        INSERT INTO atlas_driver_offer_bpp.person (
            id, first_name, last_name, role, gender, identifier_type,
            mobile_number_encrypted, mobile_number_hash, mobile_country_code,
            merchant_id, merchant_operating_city_id,
            onboarded_from_dashboard, is_new, total_earned_coins, used_coins, language,
            created_at, updated_at
        ) VALUES (
            drv_id, 'TestIntlDriver_' || rec.city, 'TestLast', 'DRIVER', 'MALE', 'MOBILENUMBER',
            sample_enc, sample_hash, '+358',
            rec.merchant_id, rec.id,
            false, false, 0, 0, 'ENGLISH',
            now(), now()
        );

        INSERT INTO atlas_driver_offer_bpp.driver_information (
            driver_id, active, enabled, verified, on_ride, blocked,
            num_of_locks, payment_pending, subscribed,
            can_downgrade_to_hatchback, can_downgrade_to_sedan, can_downgrade_to_taxi,
            aadhaar_verified, ac_restriction_lift_count,
            can_switch_to_intra_city, can_switch_to_inter_city, can_switch_to_rental,
            created_at, updated_at
        ) VALUES (
            drv_id, false, true, true, false, false,
            0, false, true,
            false, false, false,
            false, 0,
            true, true, true,
            now(), now()
        );

        INSERT INTO atlas_driver_offer_bpp.vehicle (
            driver_id, merchant_id, variant, model, color, registration_no,
            category, make, capacity, vehicle_class, selected_service_tiers,
            created_at, updated_at
        ) VALUES (
            drv_id, rec.merchant_id, 'SEDAN', 'Test Sedan', 'White', veh_no,
            'CAR', 'TestMake', 4, '3WT', '{SEDAN,COMFY}',
            now(), now()
        );

        INSERT INTO atlas_driver_offer_bpp.driver_stats (
            driver_id, bonus_earned, earnings_missed, idle_since,
            late_night_trips, total_distance, total_earnings, total_rides,
            fav_rider_count, updated_at
        ) VALUES (
            drv_id, 0, 0, now(),
            0, 0, 0, 0,
            0, now()
        );

        INSERT INTO atlas_driver_offer_bpp.driver_license (
            id, driver_id, merchant_id,
            consent, consent_timestamp,
            document_image_id1, document_image_id2,
            driver_dob, driver_name,
            license_expiry,
            license_number_hash, license_number_encrypted,
            verification_status, failed_rules,
            class_of_vehicles, date_of_issue, reject_reason, vehicle_category,
            created_at, updated_at
        ) VALUES (
            dl_id, drv_id, rec.merchant_id,
            true, now(),
            dl_id || '-img1', NULL,
            '1990-01-01 00:00:00+00', 'TestIntlDriver_' || rec.city,
            '2099-12-31 00:00:00+00',
            dl_hash, sample_doc_enc,
            'VALID', '{}',
            '{"MCWG", "LMV", "3W-PV"}', '2020-01-01 00:00:00+00', NULL, 'CAR',
            now(), now()
        );

        INSERT INTO atlas_driver_offer_bpp.vehicle_registration_certificate (
            id, fleet_owner_id,
            certificate_number_hash, certificate_number_encrypted,
            unencrypted_certificate_number,
            fitness_expiry,
            created_at, verification_status, updated_at, failed_rules,
            document_image_id, vehicle_variant,
            merchant_id, merchant_operating_city_id
        ) VALUES (
            rc_id, NULL,
            rc_hash, sample_doc_enc,
            'INTL-RC-' || lpad(city_idx::text, 6, '0'),
            '2099-12-31 00:00:00+00',
            now(), 'VALID', now(), '{}',
            rc_id || '-img', 'SEDAN',
            rec.merchant_id, rec.id
        );

        INSERT INTO atlas_driver_offer_bpp.driver_rc_association (
            id, driver_id, rc_id,
            is_rc_active,
            associated_on, associated_till,
            consent, consent_timestamp,
            merchant_id, merchant_operating_city_id,
            error_message
        ) VALUES (
            rc_assoc_id, drv_id, rc_id,
            true,
            now(), '2099-12-31 00:00:00+00',
            true, now(),
            rec.merchant_id, rec.id,
            NULL
        );

        INSERT INTO atlas_driver_offer_bpp.registration_token (
            id, auth_medium, auth_type, auth_value_hash, token,
            verified, auth_expiry, token_expiry, attempts,
            entity_id, entity_type, merchant_id,
            created_at, updated_at
        ) VALUES (
            'test-intl-tok-' || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 18),
            'SMS', 'OTP', '9999',
            drv_id,
            true, 3, 365, 3,
            drv_id, 'USER',
            rec.merchant_id,
            now(), now()
        );
    END LOOP;

    RAISE NOTICE '[international] Seeded % test driver(s) across BRIDGE_FINLAND_PARTNER cities', city_idx;
END $$;


-- ─────────────────────────────────────────────────────────────────────────────
-- SECTION 3: FLEET — MSIL_PARTNER cities
-- Self-contained: own driver person + driver_information + vehicle + stats +
-- driver_license + vehicle_registration_certificate (linked to fleet_owner) +
-- driver_rc_association + registration_token, plus a fleet_owner person and
-- their registration_token so fleet-onboarding flows have data to operate on.
-- ─────────────────────────────────────────────────────────────────────────────
DO $$
DECLARE
    sample_enc CONSTANT TEXT  := '0.1.0|0|j4VGXmuvt7HU5qP+En30NqGAn2z8KpRGFeJVd8H/VAWPB3mliuNjeQWteRoXUC7yYOOMjV6m0uYkJqy/Ww==';
    sample_hash CONSTANT bytea := '\x1604f4cfb781d8f1c9ff024f8d06abc265a2d6954ac59079ea0c11f9f0bc1fa4';
    sample_doc_enc CONSTANT TEXT := '0.1.0|0|j4VGXmuvt7HU5qP+En30NqGAn2z8KpRGFeJVd8H/VAWPB3mliuNjeQWteRoXUC7yYOOMjV6m0uYkJqy/Ww==';

    rec RECORD;
    city_idx INT := 0;
    drv_id TEXT;
    veh_no TEXT;
    rc_id TEXT;
    dl_id TEXT;
    rc_assoc_id TEXT;
    dl_hash bytea;
    rc_hash bytea;
    fleet_owner_id_v TEXT;
BEGIN
    FOR rec IN
        SELECT moc.id, moc.merchant_id, moc.city
        FROM atlas_driver_offer_bpp.merchant_operating_city moc
        JOIN atlas_driver_offer_bpp.merchant m ON m.id = moc.merchant_id
        WHERE m.short_id = 'MSIL_PARTNER'
        ORDER BY moc.city, moc.id
    LOOP
        city_idx := city_idx + 1;
        drv_id            := 'test-fleet-drv-'   || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 17);
        veh_no            := 'FLEET-'            || lpad(city_idx::text, 6, '0');
        dl_id             := 'test-fleet-dl-'    || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 18);
        rc_id             := 'test-fleet-rc-'    || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 18);
        rc_assoc_id       := 'test-fleet-rcas-'  || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 16);
        fleet_owner_id_v  := 'test-fleet-owner-' || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 16);
        dl_hash := sha256(('test-fleet-dl-' || city_idx)::bytea);
        rc_hash := sha256(('test-fleet-rc-' || city_idx)::bytea);

        -- Fleet owner person (created first so RC FK can point to it)
        INSERT INTO atlas_driver_offer_bpp.person (
            id, first_name, last_name, role, gender, identifier_type,
            mobile_number_encrypted, mobile_number_hash, mobile_country_code,
            merchant_id, merchant_operating_city_id,
            onboarded_from_dashboard, is_new, total_earned_coins, used_coins, language,
            created_at, updated_at
        ) VALUES (
            fleet_owner_id_v, 'TestFleetOwner_' || rec.city, 'TestLast',
            'FLEET_OWNER', 'MALE', 'MOBILENUMBER',
            sample_enc, sample_hash, '+91',
            rec.merchant_id, rec.id,
            true, false, 0, 0, 'ENGLISH',
            now(), now()
        );

        -- Fleet driver person
        INSERT INTO atlas_driver_offer_bpp.person (
            id, first_name, last_name, role, gender, identifier_type,
            mobile_number_encrypted, mobile_number_hash, mobile_country_code,
            merchant_id, merchant_operating_city_id,
            onboarded_from_dashboard, is_new, total_earned_coins, used_coins, language,
            created_at, updated_at
        ) VALUES (
            drv_id, 'TestFleetDriver_' || rec.city, 'TestLast', 'DRIVER', 'MALE', 'MOBILENUMBER',
            sample_enc, sample_hash, '+91',
            rec.merchant_id, rec.id,
            false, false, 0, 0, 'ENGLISH',
            now(), now()
        );

        INSERT INTO atlas_driver_offer_bpp.driver_information (
            driver_id, active, enabled, verified, on_ride, blocked,
            num_of_locks, payment_pending, subscribed,
            can_downgrade_to_hatchback, can_downgrade_to_sedan, can_downgrade_to_taxi,
            aadhaar_verified, ac_restriction_lift_count,
            can_switch_to_intra_city, can_switch_to_inter_city, can_switch_to_rental,
            created_at, updated_at
        ) VALUES (
            drv_id, false, true, true, false, false,
            0, false, true,
            false, false, false,
            false, 0,
            true, true, true,
            now(), now()
        );

        INSERT INTO atlas_driver_offer_bpp.vehicle (
            driver_id, merchant_id, variant, model, color, registration_no,
            category, make, capacity, vehicle_class, selected_service_tiers,
            created_at, updated_at
        ) VALUES (
            drv_id, rec.merchant_id, 'SEDAN', 'Test Sedan', 'White', veh_no,
            'CAR', 'TestMake', 4, '3WT', '{SEDAN,COMFY}',
            now(), now()
        );

        INSERT INTO atlas_driver_offer_bpp.driver_stats (
            driver_id, bonus_earned, earnings_missed, idle_since,
            late_night_trips, total_distance, total_earnings, total_rides,
            fav_rider_count, updated_at
        ) VALUES (
            drv_id, 0, 0, now(),
            0, 0, 0, 0,
            0, now()
        );

        INSERT INTO atlas_driver_offer_bpp.driver_license (
            id, driver_id, merchant_id,
            consent, consent_timestamp,
            document_image_id1, document_image_id2,
            driver_dob, driver_name,
            license_expiry,
            license_number_hash, license_number_encrypted,
            verification_status, failed_rules,
            class_of_vehicles, date_of_issue, reject_reason, vehicle_category,
            created_at, updated_at
        ) VALUES (
            dl_id, drv_id, rec.merchant_id,
            true, now(),
            dl_id || '-img1', NULL,
            '1990-01-01 00:00:00+00', 'TestFleetDriver_' || rec.city,
            '2099-12-31 00:00:00+00',
            dl_hash, sample_doc_enc,
            'VALID', '{}',
            '{"MCWG", "LMV", "3W-PV"}', '2020-01-01 00:00:00+00', NULL, 'CAR',
            now(), now()
        );

        INSERT INTO atlas_driver_offer_bpp.vehicle_registration_certificate (
            id, fleet_owner_id,
            certificate_number_hash, certificate_number_encrypted,
            unencrypted_certificate_number,
            fitness_expiry,
            created_at, verification_status, updated_at, failed_rules,
            document_image_id, vehicle_variant,
            merchant_id, merchant_operating_city_id
        ) VALUES (
            rc_id, fleet_owner_id_v,
            rc_hash, sample_doc_enc,
            'FLEET-RC-' || lpad(city_idx::text, 6, '0'),
            '2099-12-31 00:00:00+00',
            now(), 'VALID', now(), '{}',
            rc_id || '-img', 'SEDAN',
            rec.merchant_id, rec.id
        );

        INSERT INTO atlas_driver_offer_bpp.driver_rc_association (
            id, driver_id, rc_id,
            is_rc_active,
            associated_on, associated_till,
            consent, consent_timestamp,
            merchant_id, merchant_operating_city_id,
            error_message
        ) VALUES (
            rc_assoc_id, drv_id, rc_id,
            true,
            now(), '2099-12-31 00:00:00+00',
            true, now(),
            rec.merchant_id, rec.id,
            NULL
        );

        -- Driver registration_token
        INSERT INTO atlas_driver_offer_bpp.registration_token (
            id, auth_medium, auth_type, auth_value_hash, token,
            verified, auth_expiry, token_expiry, attempts,
            entity_id, entity_type, merchant_id,
            created_at, updated_at
        ) VALUES (
            'test-fleet-drv-tok-' || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 13),
            'SMS', 'OTP', '9999',
            drv_id,
            true, 3, 365, 3,
            drv_id, 'USER',
            rec.merchant_id,
            now(), now()
        );

        -- Fleet owner registration_token (so dashboard can impersonate them)
        INSERT INTO atlas_driver_offer_bpp.registration_token (
            id, auth_medium, auth_type, auth_value_hash, token,
            verified, auth_expiry, token_expiry, attempts,
            entity_id, entity_type, merchant_id,
            created_at, updated_at
        ) VALUES (
            'test-fleet-own-tok-' || lpad(city_idx::text, 4, '0') || '-' || substring(rec.id, 1, 13),
            'SMS', 'OTP', '9999',
            fleet_owner_id_v,
            true, 3, 365, 3,
            fleet_owner_id_v, 'USER',
            rec.merchant_id,
            now(), now()
        );
    END LOOP;

    RAISE NOTICE '[fleet] Seeded % fleet driver(s) + fleet_owner(s) across MSIL_PARTNER cities', city_idx;
END $$;
