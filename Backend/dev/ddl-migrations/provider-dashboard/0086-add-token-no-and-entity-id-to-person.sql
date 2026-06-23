-- The shared dashboard/Lib `PersonE` type now carries tokenNo + entityId for
-- the rider-side PT-employee login flow. Provider-dashboard doesn't use those
-- fields but the Beam machinery still SELECTs them, so the columns must exist
-- in atlas_bpp_dashboard.person too. They stay NULL on the BPP side.
--
-- Deploy order: APPLY THIS MIGRATION BEFORE the new provider-dashboard binary
-- ships. The binary's Beam SELECT lists `token_no` / `entity_id`; reading any
-- existing row will 500 if the columns aren't present yet.

ALTER TABLE atlas_bpp_dashboard.person
  ADD COLUMN token_no bytea;

ALTER TABLE atlas_bpp_dashboard.person
  ADD COLUMN entity_id character varying(36);
