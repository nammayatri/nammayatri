-- Capability framework tables (dashboard unification Phase 3).
-- atlas_bpp_dashboard is substituted per dashboard side pre-merge; tables converge into
-- atlas_dashboard at Phase 1. Enforcement stays on access_matrix until the
-- Phase 4 flip; until then verifyApi only shadow-logs disagreements.

CREATE TABLE atlas_bpp_dashboard.capability (
id character varying(255) NOT NULL,
domain character varying(64) NOT NULL,
description character varying(1024) NOT NULL DEFAULT '',
is_system boolean NOT NULL DEFAULT false,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT capability_pkey PRIMARY KEY (id)
);
ALTER TABLE atlas_bpp_dashboard.capability OWNER TO atlas_bpp_dashboard_user;

CREATE TABLE atlas_bpp_dashboard.capability_endpoint (
capability_id character varying(255) NOT NULL REFERENCES atlas_bpp_dashboard.capability (id),
server_name character varying(64) NOT NULL,
endpoint_id character varying(512) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT capability_endpoint_pkey PRIMARY KEY (server_name, endpoint_id)
);
ALTER TABLE atlas_bpp_dashboard.capability_endpoint OWNER TO atlas_bpp_dashboard_user;
CREATE INDEX idx_capability_endpoint_capability ON atlas_bpp_dashboard.capability_endpoint (capability_id);

CREATE TABLE atlas_bpp_dashboard.role_capability (
role_id character(36) NOT NULL REFERENCES atlas_bpp_dashboard.role (id),
capability_id character varying(255) NOT NULL REFERENCES atlas_bpp_dashboard.capability (id),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT role_capability_pkey PRIMARY KEY (role_id, capability_id)
);
ALTER TABLE atlas_bpp_dashboard.role_capability OWNER TO atlas_bpp_dashboard_user;

CREATE TABLE atlas_bpp_dashboard.person_capability (
person_id character(36) NOT NULL REFERENCES atlas_bpp_dashboard.person (id),
capability_id character varying(255) NOT NULL REFERENCES atlas_bpp_dashboard.capability (id),
mode character varying(8) NOT NULL,
reason character varying(1024) NOT NULL,
granted_by character(36) REFERENCES atlas_bpp_dashboard.person (id),
expires_at timestamp with time zone,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT person_capability_pkey PRIMARY KEY (person_id, capability_id)
,CONSTRAINT person_capability_mode_check CHECK (mode IN ('GRANT','DENY'))
);
ALTER TABLE atlas_bpp_dashboard.person_capability OWNER TO atlas_bpp_dashboard_user;

CREATE TABLE atlas_bpp_dashboard.access_audit (
id character(36) NOT NULL,
actor_id character(36),
action character varying(255) NOT NULL,
target_type character varying(64) NOT NULL,
target_id character varying(255),
before_value text,
after_value text,
reason character varying(1024),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT access_audit_pkey PRIMARY KEY (id)
);
ALTER TABLE atlas_bpp_dashboard.access_audit OWNER TO atlas_bpp_dashboard_user;
CREATE INDEX idx_access_audit_target ON atlas_bpp_dashboard.access_audit (target_type, target_id);

ALTER TABLE atlas_bpp_dashboard.person ADD COLUMN admin_tier character varying(32) NOT NULL DEFAULT 'USER';
ALTER TABLE atlas_bpp_dashboard.person ADD CONSTRAINT person_admin_tier_check
  CHECK (admin_tier IN ('SUPER_ADMIN','DASHBOARD_ADMIN','USER'));
