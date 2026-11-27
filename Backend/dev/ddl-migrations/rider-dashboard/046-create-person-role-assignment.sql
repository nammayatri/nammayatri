-- Scoped role assignments (same table as provider-dashboard 0112, for the dashboards that run the shared person writes): a person holds a role on a scope (a merchant, a city or a fleet),
-- and one person can hold several. This is what lets a fleet be a scope instead of a login:
-- the same person can be an admin of Kolkata and the admin of Fleet A at once.
--
-- scope_type / scope_id are open text, like person_resource_access.resource_type:
--   MERCHANT -> merchant id, CITY -> city name, FLEET -> fleet id (the fleet owner's person id).
-- status: INVITED (offered, not yet accepted), ACTIVE, REVOKED. Only ACTIVE grants access.
-- The same city name can exist in two merchants, so merchant_id is part of the unique key.
-- handover_from: on accept, that person's active assignment on the same fleet is revoked.

CREATE TABLE IF NOT EXISTS atlas_dashboard.person_role_assignment (
id character(36) NOT NULL,
person_id character(36) NOT NULL REFERENCES atlas_dashboard.person (id),
role_id character(36) NOT NULL REFERENCES atlas_dashboard.role (id),
scope_type character varying(32) NOT NULL,
scope_id character varying(255) NOT NULL,
merchant_id character(36) NOT NULL REFERENCES atlas_dashboard.merchant (id),
status character varying(16) NOT NULL DEFAULT 'ACTIVE',
granted_by character(36) REFERENCES atlas_dashboard.person (id) ON DELETE SET NULL,
handover_from character(36) REFERENCES atlas_dashboard.person (id) ON DELETE SET NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT person_role_assignment_pkey PRIMARY KEY (id)
,CONSTRAINT unique_person_role_scope UNIQUE (person_id, role_id, scope_type, scope_id, merchant_id)
);
ALTER TABLE atlas_dashboard.person_role_assignment OWNER TO atlas_dashboard_user;
CREATE INDEX IF NOT EXISTS idx_person_role_assignment_person
  ON atlas_dashboard.person_role_assignment (person_id, status);
CREATE INDEX IF NOT EXISTS idx_person_role_assignment_scope
  ON atlas_dashboard.person_role_assignment (scope_type, scope_id, status);

-- 0109's default privileges only cover tables created by the role that ran it, so grant
-- explicitly: both application servers verify sessions (read); driver-app also writes, because
-- the login and user-administration tree runs there.
DO $$
DECLARE app_user TEXT;
BEGIN
  FOREACH app_user IN ARRAY ARRAY['atlas_app_user', 'atlas_driver_offer_bpp_user'] LOOP
    IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = app_user) THEN
      EXECUTE format('GRANT SELECT ON atlas_dashboard.person_role_assignment TO %I', app_user);
    END IF;
  END LOOP;
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'atlas_driver_offer_bpp_user') THEN
    GRANT INSERT, UPDATE, DELETE ON atlas_dashboard.person_role_assignment TO atlas_driver_offer_bpp_user;
  END IF;
END $$;
