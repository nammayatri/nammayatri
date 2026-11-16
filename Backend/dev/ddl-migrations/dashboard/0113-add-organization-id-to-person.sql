ALTER TABLE atlas_dashboard.person
  ADD COLUMN IF NOT EXISTS organization_id text;
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'atlas_app_user') THEN
    GRANT UPDATE (organization_id, updated_at) ON atlas_dashboard.person TO atlas_app_user;
  END IF;
END $$;
