-- The shared dashboard/Lib `PersonE` type now carries token_no_hash + entityId for
-- the rider-side PT-employee login flow. Provider-dashboard doesn't use those
-- fields but the Beam machinery still SELECTs them, so the columns must exist
-- in atlas_bpp_dashboard.person too. They stay NULL on the BPP side.
--
-- Deploy order: APPLY THIS MIGRATION BEFORE the new provider-dashboard binary
-- ships. The binary's Beam SELECT lists `token_no_hash` / `entity_id`; reading any
-- existing row will 500 if the columns aren't present yet.

ALTER TABLE atlas_bpp_dashboard.person
  ADD COLUMN token_no_hash bytea;

ALTER TABLE atlas_bpp_dashboard.person
  ADD COLUMN entity_id character varying(36);

-- Both columns exist ONLY to satisfy the shared Beam binding. Provider-dashboard
-- has no PT-employee flow and no `entity` table, so any non-NULL value would
-- either be a bug or dangle when read. Enforce at the schema level.
ALTER TABLE atlas_bpp_dashboard.person
  ADD CONSTRAINT person_pt_fields_null_on_bpp
  CHECK (token_no_hash IS NULL AND entity_id IS NULL);
