-- The shared dashboard/Lib `PersonT` type now carries tokenNo + entityId for
-- the rider-side PT-employee login flow. Safety-dashboard doesn't use those
-- fields but the Beam machinery still SELECTs them, so the columns must exist
-- in atlas_safety_dashboard.person too. They stay NULL on the safety side.
--
-- Deploy order: APPLY THIS MIGRATION BEFORE the new safety-dashboard binary
-- ships. The binary's Beam SELECT lists `token_no` / `entity_id`; reading any
-- existing row will 500 if the columns aren't present yet.

ALTER TABLE atlas_safety_dashboard.person
  ADD COLUMN token_no bytea;

ALTER TABLE atlas_safety_dashboard.person
  ADD COLUMN entity_id character varying(36);
