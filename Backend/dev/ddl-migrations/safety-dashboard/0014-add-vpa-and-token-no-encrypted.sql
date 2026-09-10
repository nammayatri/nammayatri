-- The shared dashboard/Lib `PersonT` type now carries vpa_encrypted + vpa_hash
-- and token_no_encrypted for the PT-employee flow. Safety-dashboard doesn't use
-- these fields but the Beam machinery still SELECTs the columns, so they must
-- exist in atlas_safety_dashboard.person too. They stay NULL on the safety side.
--
-- tokenNo uniqueness is per-merchant and enforced at the application level
-- (per-merchant bulkCreate lock + findTokenNoConflictForMerchant), not at the DB
-- level — safety-dashboard has no PT-employee flow, so no constraint is needed.
--
-- Deploy order: APPLY THIS MIGRATION BEFORE the new safety-dashboard binary
-- ships. The binary's Beam SELECT lists `vpa_encrypted` / `vpa_hash` /
-- `token_no_encrypted`; reading any existing row will 500 if the columns aren't
-- present yet.

ALTER TABLE atlas_safety_dashboard.person
  ADD COLUMN IF NOT EXISTS vpa_encrypted text;

ALTER TABLE atlas_safety_dashboard.person
  ADD COLUMN IF NOT EXISTS vpa_hash bytea;

ALTER TABLE atlas_safety_dashboard.person
  ADD COLUMN IF NOT EXISTS token_no_encrypted text;

-- Index (not unique) is harmless here; column is always NULL on the safety side.
CREATE INDEX IF NOT EXISTS person_token_no_hash_idx ON atlas_safety_dashboard.person (token_no_hash);
