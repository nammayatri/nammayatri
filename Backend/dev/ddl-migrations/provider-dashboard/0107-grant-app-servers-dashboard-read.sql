-- Let the application servers use the dashboard database.
--
-- rider-app and dynamic-offer-driver-app now verify a dashboard operator's
-- session and capabilities themselves rather than being proxied by
-- provider-dashboard, and they serve the login / user-administration tree
-- (lib-dashboard's API.DashboardLogin) directly. They connect to
-- atlas_dashboard with their OWN database users, which have no privileges here
-- -- every direct request fails with "permission denied for schema
-- atlas_dashboard" (SQLSTATE 42501) until this grant is applied.
--
-- Two levels of privilege, because two different things run here:
--
--   Read + audit   -- verifying a session and recording who called what. This
--                     is all the merchant-scoped dashboard routes need.
--   Write          -- the login tree mutates dashboard state: registration_token
--                     on login/logout, person on profile/password/role changes,
--                     role, merchant_access, person_capability,
--                     person_resource_access, deleted_user, access_matrix.
--
-- The write grant is a real widening: an application server can now modify
-- dashboard users and roles. That is inherent in serving user administration
-- from the application servers rather than proxying it -- they run the same
-- code provider-dashboard runs, so they need the same privileges. If that is
-- not wanted, the login tree should stay on provider-dashboard and only the
-- read + audit block below should be applied.

DO $$
DECLARE app_user TEXT;
BEGIN
  FOREACH app_user IN ARRAY ARRAY['atlas_app_user', 'atlas_driver_offer_bpp_user'] LOOP
    IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = app_user) THEN
      -- read + audit
      EXECUTE format('GRANT USAGE ON SCHEMA atlas_dashboard TO %I', app_user);
      EXECUTE format('GRANT SELECT ON ALL TABLES IN SCHEMA atlas_dashboard TO %I', app_user);
      EXECUTE format('GRANT INSERT ON atlas_dashboard.transaction TO %I', app_user);

      -- write: required only by the login / user-administration tree
      EXECUTE format('GRANT INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA atlas_dashboard TO %I', app_user);
      EXECUTE format('GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA atlas_dashboard TO %I', app_user);

      -- tables added later must stay usable without revisiting this migration
      EXECUTE format('ALTER DEFAULT PRIVILEGES IN SCHEMA atlas_dashboard GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO %I', app_user);
      EXECUTE format('ALTER DEFAULT PRIVILEGES IN SCHEMA atlas_dashboard GRANT USAGE, SELECT ON SEQUENCES TO %I', app_user);
    END IF;
  END LOOP;
END $$;
