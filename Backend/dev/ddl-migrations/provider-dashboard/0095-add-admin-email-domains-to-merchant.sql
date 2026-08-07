ALTER TABLE atlas_bpp_dashboard.merchant
  ADD COLUMN IF NOT EXISTS admin_email_domains text[] NOT NULL DEFAULT '{}';
