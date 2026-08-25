-- Tombstone/audit of deleted dashboard users. Holds a snapshot of the person at
-- deletion time (no FK on person_id/role_id/deleted_by — the person row is gone).
CREATE TABLE atlas_bap_dashboard.deleted_user (
  id character(36) NOT NULL,
  person_id character(36) NOT NULL,
  first_name character varying(255) NOT NULL,
  last_name character varying(255) NOT NULL,
  role_id character(36) NOT NULL,
  email_encrypted character varying(255),
  deleted_by character(36) NOT NULL,
  deleted_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
  CONSTRAINT deleted_user_pkey PRIMARY KEY (id)
);
ALTER TABLE atlas_bap_dashboard.deleted_user OWNER TO atlas_bap_dashboard_user;

-- Keep granted_by as historical data even after the grantor is deleted: drop the
-- FK so a deleted grantor no longer blocks person deletion (the id stays resolvable
-- via deleted_user). person_capability.person_id (subject) rows are still deleted
-- by the app before the person delete.
ALTER TABLE atlas_bap_dashboard.person_capability DROP CONSTRAINT IF EXISTS person_capability_granted_by_fkey;
ALTER TABLE atlas_bap_dashboard.deleted_user ADD COLUMN IF NOT EXISTS delete_reason text;
