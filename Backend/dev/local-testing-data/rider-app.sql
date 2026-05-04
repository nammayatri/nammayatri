-- Local-testing seed for rider-app (atlas_app).
--
-- Inserts ONE seed Person + ONE RegistrationToken per merchant present in
-- atlas_app.merchant. Iterates via CROSS JOIN so adding a new merchant via
-- config-sync automatically extends this seed on the next apply.
--
-- All seed riders share the same mobile number (mobile-uniqueness is not
-- enforced on atlas_app.person). The mobile is stored encrypted+hashed using
-- passetto + the codebase's standard hash salt.
--
-- Encryption: passetto /encrypt (http://127.0.0.1:8079/encrypt). The mobile
-- below decrypts to "9999900001".
-- Hash: sha256("How wonderful it is that nobody need wait a single moment
-- before starting to improve the world" || "9999900001"), hex-encoded.
--
-- Idempotent via ON CONFLICT DO NOTHING — safe to re-apply.

-- unencrypted: 9999900001
INSERT INTO atlas_app.person
  ( id
  , blocked
  , gender
  , has_taken_valid_ride
  , identifier_type
  , is_new
  , is_valid_rating
  , role
  , merchant_id
  , mobile_country_code
  , mobile_number_encrypted
  , mobile_number_hash
  , unencrypted_mobile_number
  , first_name
  , last_name
  , created_at
  , updated_at
  )
SELECT
    md5(m.id || ':seed-rider-person')::uuid::text
  , false
  , 'UNKNOWN'
  , false
  , 'MOBILENUMBER'
  , false
  , true
  , 'USER'
  , m.id
  , '+91'
    -- unencrypted: 9999900001
  , '0.1.0|1|pw8GaBhQDD7vibd+HR13eV1JNYGZ8WBb0w2b6yUC+FJVf3jkvDw+whhLUn78e37JcRT7NaXwQhJBVjRKQQ=='
  , decode('631c7fc20076835796866f0a319e6d7e2ffb08096495cce4c193a9dbe8d20199', 'hex')
  , '9999900001'
  , 'seed_rider'
  , m.short_id
  , now()
  , now()
FROM atlas_app.merchant m
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_app.person p
  WHERE p.id = md5(m.id || ':seed-rider-person')::uuid::text
);

-- One RegistrationToken per seed rider Person. Token value is deterministic
-- per merchant: 'seed-rider-token-<merchant_short_id>'.
INSERT INTO atlas_app.registration_token
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
    md5(m.id || ':seed-rider-token')::uuid::text
  , 0
  , 365
  , 'SMS'
  , 'OTP'
  , '7891'
  , md5(m.id || ':seed-rider-person')::uuid::text
  , 'USER'
  , m.id
  , 'seed-rider-token-' || m.short_id
  , 365
  , true
  , now()
  , now()
FROM atlas_app.merchant m
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_app.registration_token rt
  WHERE rt.id = md5(m.id || ':seed-rider-token')::uuid::text
);
