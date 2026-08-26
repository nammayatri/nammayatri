ALTER TABLE atlas_bap_dashboard.person
  ADD COLUMN IF NOT EXISTS force_password_change boolean;
