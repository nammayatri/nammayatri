-- The shared dashboard/Lib `PersonT` type now carries token_no_hash + entityId for
-- the rider-side PT-employee login flow. Safety-dashboard doesn't use those
-- fields but the Beam machinery still SELECTs them, so the columns must exist
-- in atlas_safety_dashboard.person too. They stay NULL on the safety side.
--
-- Deploy order: APPLY THIS MIGRATION BEFORE the new safety-dashboard binary
-- ships. The binary's Beam SELECT lists `token_no_hash` / `entity_id`; reading any
-- existing row will 500 if the columns aren't present yet.

ALTER TABLE atlas_safety_dashboard.person
  ADD COLUMN token_no_hash bytea;

ALTER TABLE atlas_safety_dashboard.person
  ADD COLUMN entity_id character varying(36);

-- Both columns exist ONLY to satisfy the shared Beam binding. Safety-dashboard
-- has no PT-employee flow and no `entity` table, so any non-NULL value would
-- either be a bug or dangle when read. Enforce at the schema level.
ALTER TABLE atlas_safety_dashboard.person
  ADD CONSTRAINT person_pt_fields_null_on_safety
  CHECK (token_no_hash IS NULL AND entity_id IS NULL);
