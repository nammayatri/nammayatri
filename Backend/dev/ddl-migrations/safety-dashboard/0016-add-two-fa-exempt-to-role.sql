ALTER TABLE atlas_dashboard.role
  ADD COLUMN IF NOT EXISTS two_fa_exempt boolean NOT NULL DEFAULT false;
