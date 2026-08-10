-- PT employee (conductor / depot manager) login support, carried over from
-- rider-dashboard/0058 as part of the dashboard unification.
--
-- The unified backend is provider-dashboard and it now serves the BAP route
-- tree, including PT_EMPLOYEE logins — but the entity table and the two person
-- columns behind them only ever existed in the rider-dashboard migration set.
-- Production got them when atlas_dashboard was built; this file is what gives a
-- freshly-migrated local database the same shape.
--
-- Idempotent, because prod already has these objects.

ALTER TABLE atlas_dashboard.person
  ADD COLUMN IF NOT EXISTS token_no_hash bytea;

ALTER TABLE atlas_dashboard.person
  ADD COLUMN IF NOT EXISTS entity_id character varying(36);

CREATE TABLE IF NOT EXISTS atlas_dashboard.entity (
  id              character varying(36) NOT NULL,
  merchant_id     character varying(36) NOT NULL,
  entity_name     text NOT NULL,
  entity_short_id character varying(36) NOT NULL,
  deleted         boolean NOT NULL DEFAULT false,
  created_at      timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at      timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT entity_pkey PRIMARY KEY (id),
  CONSTRAINT entity_short_id_unique_per_merchant UNIQUE (merchant_id, entity_short_id)
);

ALTER TABLE atlas_dashboard.entity OWNER TO atlas_dashboard_user;
