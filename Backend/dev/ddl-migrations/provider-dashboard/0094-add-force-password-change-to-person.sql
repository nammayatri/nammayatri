ALTER TABLE atlas_bpp_dashboard.person
  ADD COLUMN IF NOT EXISTS force_password_change boolean;
