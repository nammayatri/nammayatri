ALTER TABLE atlas_bpp_dashboard.merchant
  ADD COLUMN track_login_logout_for_roles text[] NOT NULL DEFAULT '{}';
