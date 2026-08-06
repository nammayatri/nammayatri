ALTER TABLE atlas_bpp_dashboard.merchant
  ADD COLUMN admin_email_domains text[] NOT NULL DEFAULT '{}';