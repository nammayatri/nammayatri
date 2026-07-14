-- Local-testing seed for dynamic-offer-driver-app (atlas_driver_offer_bpp).
--
-- Inserts ONE seed driver Person + RegistrationToken + DriverInformation +
-- Vehicle + VehicleRegistrationCertificate + DriverLicense per merchant
-- present in atlas_driver_offer_bpp.merchant. Adds a DriverBankAccount only
-- for the BRIDGE_FINLAND merchant (international flow needs Stripe-style
-- bank-account linkage).
--
-- All seed drivers share the same mobile number; the same encrypted blob
-- + hash is reused across merchants. Encrypted values are produced by
-- passetto /encrypt; hashes are sha256(SALT || value) where
-- SALT = "How wonderful it is that nobody need wait a single moment before
-- starting to improve the world".
--
-- Idempotent via NOT EXISTS — safe to re-apply.

-- ────────────────────────────────────────────────────────────────────────
-- 1. Person (driver)
-- ────────────────────────────────────────────────────────────────────────

-- unencrypted: 9999900002
-- total_ratings / total_rating_score / is_valid_rating are added by a later
-- migration and are nullable in the latest schema; omit them so the seed
-- applies on a freshly-migrated dev DB regardless of feature-migration order.
INSERT INTO atlas_driver_offer_bpp.person
  ( id
  , first_name
  , last_name
  , gender
  , identifier_type
  , is_new
  , merchant_id
  , onboarded_from_dashboard
  , role
  , total_earned_coins
  , used_coins
  , mobile_country_code
  , mobile_number_encrypted
  , mobile_number_hash
  , unencrypted_mobile_number
  , created_at
  , updated_at
  )
SELECT
    md5(m.id || ':seed-driver-person')::uuid::text
  , 'seed_driver'
  , m.short_id
  , 'UNKNOWN'
  , 'MOBILENUMBER'
  , false
  , m.id
  , false
  , 'DRIVER'
  , 0
  , 0
  , '+91'
    -- unencrypted: 9999900002
  , '0.1.0|0|LrtmhWFOUgWcOe/iKCDi4NMKtwGF5qRvs2H4J+b4XnsPFEbe5CTnqXf5qibMMj571UTQyJNF3g+92aMvXQ=='
  , decode('490d8b0e5644d68edb5e586b98a97444e3e54fcf778a2d627e2451e386d56269', 'hex')
  , '9999900002'
  , now()
  , now()
FROM atlas_driver_offer_bpp.merchant m
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.person p
  WHERE p.id = md5(m.id || ':seed-driver-person')::uuid::text
);

-- ────────────────────────────────────────────────────────────────────────
-- 2. RegistrationToken (driver)
-- entity_id is character(36) — pad/extend the UUID-text to exactly 36 chars
-- (UUID with dashes is already 36 chars, so the cast suffices).
-- ────────────────────────────────────────────────────────────────────────
INSERT INTO atlas_driver_offer_bpp.registration_token
  ( id
  , attempts
  , auth_expiry
  , auth_medium
  , auth_type
  , auth_value_hash
  , entity_id
  , entity_type
  , merchant_id
  , token
  , token_expiry
  , verified
  , created_at
  , updated_at
  )
SELECT
    md5(m.id || ':seed-driver-token')::uuid::text
  , 0
  , 365
  , 'SMS'
  , 'OTP'
  , '7891'
  , md5(m.id || ':seed-driver-person')::uuid::text
  , 'USER                              '
  , m.id
  , 'seed-driver-token-' || m.short_id
  , 365
  , true
  , now()
  , now()
FROM atlas_driver_offer_bpp.merchant m
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.registration_token rt
  WHERE rt.id = md5(m.id || ':seed-driver-token')::uuid::text
);

-- ────────────────────────────────────────────────────────────────────────
-- 3. DriverInformation
-- driver_id is the PK; one row per driver. All other NOT NULL columns have
-- defaults (active, blocked, enabled, etc.) so we only insert driver_id +
-- merchant_id + the two cities/IDs.
-- ────────────────────────────────────────────────────────────────────────
INSERT INTO atlas_driver_offer_bpp.driver_information
  ( driver_id
  , merchant_id
  , enabled
  , verified
  , active
  , subscribed
  , created_at
  , updated_at
  )
SELECT
    md5(m.id || ':seed-driver-person')::uuid::text
  , m.id
  , true
  , true
  , true
  , true
  , now()
  , now()
FROM atlas_driver_offer_bpp.merchant m
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.driver_information di
  WHERE di.driver_id = md5(m.id || ':seed-driver-person')::uuid::text
);

-- ────────────────────────────────────────────────────────────────────────
-- 3b. DriverStats
-- PK = driver_id; only driver_id and idle_since are NOT NULL without
-- defaults. All counters/earnings default to 0.
-- ────────────────────────────────────────────────────────────────────────
INSERT INTO atlas_driver_offer_bpp.driver_stats
  ( driver_id
  , idle_since
  , updated_at
  )
SELECT
    md5(m.id || ':seed-driver-person')::uuid::text
  , now()
  , now()
FROM atlas_driver_offer_bpp.merchant m
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.driver_stats ds
  WHERE ds.driver_id = md5(m.id || ':seed-driver-person')::uuid::text
);

-- ────────────────────────────────────────────────────────────────────────
-- 4. Vehicle (PK = driver_id, so one per driver)
-- registration_no is unique per merchant via short_id concatenation.
-- ────────────────────────────────────────────────────────────────────────
INSERT INTO atlas_driver_offer_bpp.vehicle
  ( driver_id
  , merchant_id
  , color
  , model
  , registration_no
  , variant
  , vehicle_class
  , category
  , make
  , capacity
  , created_at
  , updated_at
  )
SELECT
    md5(m.id || ':seed-driver-person')::uuid::text
  , m.id
  , 'White'
  , 'Swift Dzire'
  , 'SEED-' || m.short_id || '-0001'
  , 'SEDAN'
  , 'M1'
  , 'CAR'
  , 'Maruti'
  , 4
  , now()
  , now()
FROM atlas_driver_offer_bpp.merchant m
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.vehicle v
  WHERE v.driver_id = md5(m.id || ':seed-driver-person')::uuid::text
);

-- ────────────────────────────────────────────────────────────────────────
-- 5. VehicleRegistrationCertificate
-- One RC per driver. certificate_number_encrypted is a fixed passetto blob
-- (decrypts to "KA01SD0001"). certificate_number_hash is the precomputed
-- sha256(salt || "KA01SD0001") — same fixed value across all merchants;
-- there is no UNIQUE constraint on the hash so collisions are fine.
-- (Inline pgcrypto.digest() was tried first, but pgcrypto is not installed
-- in the dev DB; precomputed bytea literals avoid the dependency.)
-- ────────────────────────────────────────────────────────────────────────
-- The plaintext `certificate_number` column was removed in a later
-- migration (current schema keeps only `*_encrypted` + `*_hash`).
-- Omit it so this seed applies on a freshly-migrated dev DB regardless
-- of feature-migration order.
INSERT INTO atlas_driver_offer_bpp.vehicle_registration_certificate
  ( id
  , document_image_id
  , fitness_expiry
  , verification_status
  , certificate_number_hash
  , certificate_number_encrypted
  , failed_rules
  , vehicle_class
  , vehicle_manufacturer
  , vehicle_model
  , vehicle_variant
  , vehicle_color
  , vehicle_capacity
  , merchant_id
  , created_at
  , updated_at
  )
SELECT
    md5(m.id || ':seed-driver-rc')::uuid::text
  , md5(m.id || ':seed-driver-rc-img')::uuid::text
  , now() + interval '365 days'
  , 'VALID'
    -- Unique-per-merchant hash via md5(salt || "<short_id>-RC0001"). md5 is
    -- a Postgres builtin (pgcrypto isn't installed on this dev DB) and the
    -- per-merchant short_id makes the hash unique across merchants, satisfying
    -- the UNIQUE constraint `unique_rc_id` on certificate_number_hash.
  , decode(md5('How wonderful it is that nobody need wait a single moment before starting to improve the world' || (m.short_id || '-RC0001')), 'hex')
    -- unencrypted: KA01SD0001 (placeholder; same blob reused across merchants)
  , '0.1.0|1|/tDOHKVLfkdl6kbUbX7vIh1Zv3UC6CuFID4XaTA83DmFvcXp+QS6Om8oCE436e/4HRK4YpV+8uh5ZJi+Nw=='
  , ARRAY[]::text[]
  , 'M1'
  , 'Maruti'
  , 'Swift Dzire'
  , 'SEDAN'
  , 'White'
  , 4
  , m.id
  , now()
  , now()
FROM atlas_driver_offer_bpp.merchant m
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.vehicle_registration_certificate rc
  WHERE rc.id = md5(m.id || ':seed-driver-rc')::uuid::text
);

-- ────────────────────────────────────────────────────────────────────────
-- 6. DriverLicense
-- license_number_encrypted is a fixed passetto blob (decrypts to
-- "DLSEED000000001"); license_number_hash is the precomputed
-- sha256(salt || "DLSEED000000001"). No UNIQUE constraint, so a shared
-- hash across merchants is fine.
-- ────────────────────────────────────────────────────────────────────────
-- Same migration-skew pattern as RC: plaintext `license_number` column
-- was removed; only `license_number_encrypted` + `_hash` remain. Omit
-- the plaintext column so the seed applies on fresh DBs.
INSERT INTO atlas_driver_offer_bpp.driver_license
  ( id
  , consent
  , consent_timestamp
  , document_image_id1
  , driver_id
  , license_expiry
  , verification_status
  , license_number_hash
  , license_number_encrypted
  , failed_rules
  , class_of_vehicles
  , merchant_id
  , created_at
  , updated_at
  )
SELECT
    md5(m.id || ':seed-driver-license')::uuid::text
  , true
  , now()
  , md5(m.id || ':seed-driver-license-img')::uuid::text
  , md5(m.id || ':seed-driver-person')::uuid::text
  , now() + interval '730 days'
  , 'VALID'
    -- Unique-per-merchant hash via md5(salt || "<short_id>-DL0001") — same
    -- pattern as the RC hash; md5 is a Postgres builtin so this avoids the
    -- pgcrypto dependency. Likely UNIQUE constraint on license_number_hash.
  , decode(md5('How wonderful it is that nobody need wait a single moment before starting to improve the world' || (m.short_id || '-DL0001')), 'hex')
    -- unencrypted: DLSEED000000001 (placeholder; same blob reused across merchants)
  , '0.1.0|2|ivjDC2BFw3unP35pPtau/rj5xYl3XVldjgPK6fUISWhJTgg6MhL57KTwcEbJ/1R4JdNUfKJ2DIRhxYXhkxmpaWJc'
  , ARRAY[]::text[]
  , ARRAY['LMV']::text[]
  , m.id
  , now()
  , now()
FROM atlas_driver_offer_bpp.merchant m
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.driver_license dl
  WHERE dl.id = md5(m.id || ':seed-driver-license')::uuid::text
);

-- ────────────────────────────────────────────────────────────────────────
-- 7. DriverBankAccount — only for BRIDGE_FINLAND
-- International merchants (Finland, Stripe-Connect-style) require a bank
-- account linked to the driver. Indian merchants don't, so this is gated by
-- short_id = 'BRIDGE_FINLAND'.
-- ────────────────────────────────────────────────────────────────────────
INSERT INTO atlas_driver_offer_bpp.driver_bank_account
  ( driver_id
  , account_id
  , charges_enabled
  , details_submitted
  , merchant_id
  , payment_mode
  , name_at_bank
  , ifsc_code
  , created_at
  , updated_at
  )
SELECT
    md5(m.id || ':seed-driver-person')::uuid::text
  , 'acct_seed_' || m.short_id
  , true
  , true
  , m.id
  , 'TEST'
  , 'Seed Driver'
  , 'NDEAFIHH'
  , now()
  , now()
FROM atlas_driver_offer_bpp.merchant m
WHERE m.short_id IN ('BRIDGE_FINLAND_PARTNER', 'BRIDGE_CABS_PARTNER')
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.driver_bank_account dba
    WHERE dba.driver_id = md5(m.id || ':seed-driver-person')::uuid::text
  );
