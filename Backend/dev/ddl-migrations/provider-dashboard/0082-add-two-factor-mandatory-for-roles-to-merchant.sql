ALTER TABLE atlas_bpp_dashboard.merchant
  ADD COLUMN two_factor_mandatory_for_roles text[] NOT NULL DEFAULT '{}';
