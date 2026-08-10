ALTER TABLE atlas_safety_dashboard.person
  ADD COLUMN IF NOT EXISTS force_password_change boolean;
