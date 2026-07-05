ALTER TABLE atlas_bap_dashboard.merchant
  DROP COLUMN IF EXISTS two_factor_mandatory_for_roles;

ALTER TABLE atlas_bap_dashboard.merchant
  ADD COLUMN IF NOT EXISTS enforcement_deadline timestamp with time zone;
