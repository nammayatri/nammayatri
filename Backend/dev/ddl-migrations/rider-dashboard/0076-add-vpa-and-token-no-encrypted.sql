-- VPA (Virtual Payment Address) and encrypted tokenNo on person, both stored
-- encrypted + hashed the same way as email.
--
-- tokenNo uniqueness is per-merchant (same tokenNo allowed under different
-- merchants), enforced at the application level inside the per-merchant
-- bulkCreate Redis lock via `findTokenNoConflictForMerchant` — NOT via a global
-- UNIQUE constraint, because `person` carries no merchantId and the same tokenNo
-- is intentionally reusable across merchants.
--
-- Deploy order: APPLY THIS MIGRATION BEFORE the new rider-dashboard binary
-- ships. The binary's Beam SELECT lists `vpa_encrypted` / `vpa_hash` /
-- `token_no_encrypted`; reading any existing row will 500 if the columns aren't
-- present yet.

ALTER TABLE atlas_bap_dashboard.person
  ADD COLUMN IF NOT EXISTS vpa_encrypted text;

ALTER TABLE atlas_bap_dashboard.person
  ADD COLUMN IF NOT EXISTS vpa_hash bytea;

ALTER TABLE atlas_bap_dashboard.person
  ADD COLUMN IF NOT EXISTS token_no_encrypted text;

-- Index (not unique) to back the per-merchant tokenNo conflict lookup, which
-- filters on token_no_hash.
CREATE INDEX IF NOT EXISTS person_token_no_hash_idx ON atlas_bap_dashboard.person (token_no_hash);
