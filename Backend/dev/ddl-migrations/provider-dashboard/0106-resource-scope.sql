-- Layer C: resource-scoped access. person_resource_access is the allowlist of
-- resources (routes, special zones, special locations, ticket places, …) a
-- person may see/act on, under a merchant + operating city. resource_type and
-- resource_id are open text tags (no enum / no CHECK) so a new resource kind is
-- pure data. A '*' resource_id row = full-MOC; no rows = deny-all (ops gate) /
-- unscoped (analytics). Exposed to consumers via GET /user/resourceScope.

CREATE TABLE atlas_dashboard.person_resource_access (
id character(36) NOT NULL,
person_id character(36) NOT NULL REFERENCES atlas_dashboard.person (id),
merchant_id character(36) NOT NULL REFERENCES atlas_dashboard.merchant (id),
operating_city character varying(255) NOT NULL,
resource_type character varying(64) NOT NULL,
resource_id character varying(255) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT person_resource_access_pkey PRIMARY KEY (id)
,CONSTRAINT unique_person_merchant_city_type_resource UNIQUE (person_id, merchant_id, operating_city, resource_type, resource_id)
);
ALTER TABLE atlas_dashboard.person_resource_access OWNER TO atlas_dashboard_user;
CREATE INDEX idx_person_resource_access_lookup
  ON atlas_dashboard.person_resource_access (person_id, merchant_id, operating_city, resource_type);

-- Ops-gate switch: a capability with resource_type set is resource-scoped to
-- that type (open text tag, NULL = ordinary capability). Enforced by
-- Tools.Auth.Capability.enforceResourceScope. Set per capability as data, e.g.:
--   UPDATE atlas_dashboard.capability SET resource_type = 'SPECIAL_LOCATION'
--     WHERE id = 'city-config.geo.write';
ALTER TABLE atlas_dashboard.capability ADD COLUMN IF NOT EXISTS resource_type character varying(64);

-- Per-endpoint binding: WHERE a scoped endpoint carries its resource id. The id is
-- read ONLY from a path capture — 'param:<name>' → the path segment right after
-- <name> (e.g. /specialLocation/{specialLocationId}/gates/upsert → 'param:specialLocation').
-- '__SKIP__' → not gated (list/read); '__HANDLER__' → id not in the URL (CSV / DB
-- indirection) → the handler enforces; NULL → nothing to resolve → the gate logs and
-- passes. Read by verifyApi's generic gate (Tools.Auth.Capability). Set as data, e.g.:
--   UPDATE atlas_dashboard.capability_endpoint SET resource_id_param = 'param:specialLocation'
--     WHERE endpoint_id = '...UPSERT_SPECIAL_LOCATION_GATE';
ALTER TABLE atlas_dashboard.capability_endpoint ADD COLUMN IF NOT EXISTS resource_id_param character varying(64);
