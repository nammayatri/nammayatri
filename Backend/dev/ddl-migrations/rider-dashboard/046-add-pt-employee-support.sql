-- PT employee (conductor / depot manager) login support.
-- Adds token_no_hash (bytea) + entityId on the dashboard person row and introduces
-- the entity table (depots / operators) referenced by person.entity_id.
--
-- Deploy order: APPLY THIS MIGRATION BEFORE the new rider-dashboard binary
-- ships. The binary's Beam SELECT lists `token_no_hash` and `entity_id`; reading
-- any existing row will 500 if the columns aren't present yet.

ALTER TABLE atlas_bap_dashboard.person
  ADD COLUMN token_no_hash bytea;

ALTER TABLE atlas_bap_dashboard.person
  ADD COLUMN entity_id character varying(36);

CREATE TABLE atlas_bap_dashboard.entity (
  id              character varying(36) NOT NULL,
  entity_name     text NOT NULL,
  entity_short_id character varying(36) NOT NULL,
  deleted         boolean NOT NULL DEFAULT false,
  created_at      timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at      timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT entity_pkey PRIMARY KEY (id),
  CONSTRAINT entity_short_id_unique UNIQUE (entity_short_id)
);

ALTER TABLE atlas_bap_dashboard.entity OWNER TO atlas_bap_dashboard_user;
