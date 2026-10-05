ALTER TABLE atlas_dashboard.merchant
  ADD COLUMN track_login_logout_for_roles text[] NOT NULL DEFAULT '{}';
