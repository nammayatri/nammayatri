ALTER TABLE atlas_bap_dashboard.merchant
  ADD COLUMN track_login_logout_for_roles text[] NOT NULL DEFAULT '{}';

UPDATE atlas_bap_dashboard.merchant SET track_login_logout_for_roles = '{DASHBOARD_USER}' WHERE short_id = 'BHARAT_TAXI';