ALTER TABLE atlas_bap_dashboard.merchant
  ADD COLUMN admin_email_domains text[] NOT NULL DEFAULT '{}';