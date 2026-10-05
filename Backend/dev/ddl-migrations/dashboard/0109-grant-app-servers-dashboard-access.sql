-- Let the application servers use the dashboard database.
--
-- rider-app and dynamic-offer-driver-app now verify a dashboard operator's
-- session and capabilities themselves rather than being proxied by
-- provider-dashboard. They connect to atlas_dashboard with their OWN database
-- users, which have no privileges here -- every direct request fails with
-- "permission denied for schema atlas_dashboard" (SQLSTATE 42501) until this
-- grant is applied.
--
-- The two servers do different things here, so they get different grants:
--
--   rider-app   -- verifies sessions and records who called what. Session
--                  verification (Tools.Auth.Common) also deletes an expired or
--                  revoked registration_token and back-fills
--                  person.password_updated_at, so those two writes are included.
--
--   driver-app  -- additionally serves the login / user-administration tree
--                  (lib-dashboard's API.DashboardLogin), which mutates dashboard
--                  state: registration_token on login/logout, person on
--                  profile/password/role changes, role, merchant_access,
--                  person_capability, person_resource_access, deleted_user.
--                  It needs the same privileges provider-dashboard has.

DO $$
DECLARE app_user TEXT;
BEGIN
  -- Session verification and audit: both application servers.
  FOREACH app_user IN ARRAY ARRAY['atlas_app_user', 'atlas_driver_offer_bpp_user'] LOOP
    IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = app_user) THEN
      EXECUTE format('GRANT USAGE ON SCHEMA atlas_dashboard TO %I', app_user);
      EXECUTE format('GRANT SELECT ON ALL TABLES IN SCHEMA atlas_dashboard TO %I', app_user);
      EXECUTE format('GRANT INSERT ON atlas_dashboard.transaction TO %I', app_user);
      EXECUTE format('GRANT DELETE ON atlas_dashboard.registration_token TO %I', app_user);
      EXECUTE format('GRANT UPDATE (password_updated_at) ON atlas_dashboard.person TO %I', app_user);
      -- tables added later must stay readable without revisiting this migration
      EXECUTE format('ALTER DEFAULT PRIVILEGES IN SCHEMA atlas_dashboard GRANT SELECT ON TABLES TO %I', app_user);
    END IF;
  END LOOP;

  -- Login / user administration: driver-app only.
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'atlas_driver_offer_bpp_user') THEN
    GRANT INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA atlas_dashboard TO atlas_driver_offer_bpp_user;
    GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA atlas_dashboard TO atlas_driver_offer_bpp_user;
    ALTER DEFAULT PRIVILEGES IN SCHEMA atlas_dashboard GRANT INSERT, UPDATE, DELETE ON TABLES TO atlas_driver_offer_bpp_user;
    ALTER DEFAULT PRIVILEGES IN SCHEMA atlas_dashboard GRANT USAGE, SELECT ON SEQUENCES TO atlas_driver_offer_bpp_user;
  END IF;
END $$;
