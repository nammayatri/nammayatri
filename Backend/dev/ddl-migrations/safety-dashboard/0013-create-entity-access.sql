-- A depot manager may now hold more than one depot, so person-to-entity becomes 1:N and moves
-- off the person.entity_id scalar into its own grant table, mirroring merchant_access.
--
-- merchant_id is denormalised off entity so a grant can be tenancy-checked (and revoked) without
-- joining entity. The unique constraint makes re-granting the same depot idempotent rather than
-- silently duplicating rows.
--
-- person.entity_id is deliberately NOT dropped here: it is the rollback path until every replica
-- is on the new binary. The new binary neither reads nor writes it. Drop it in a follow-up once
-- this has soaked.
--
-- Deploy order: APPLY THIS MIGRATION BEFORE the new safety-dashboard binary ships. The binary
-- SELECTs entity_access; every profile and login read 500s without the table.
--
-- Safety-dashboard has no `entity` table and no PT-employee flow; the table exists only to
-- satisfy the shared dashboard/Lib Beam binding and stays empty here, so there is no backfill.


CREATE TABLE IF NOT EXISTS atlas_safety_dashboard.entity_access (
  id          character varying(36) NOT NULL,
  person_id   character varying(36) NOT NULL,
  entity_id   character varying(36) NOT NULL,
  merchant_id character varying(36) NOT NULL,
  created_at  timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT entity_access_pkey PRIMARY KEY (id),
  CONSTRAINT entity_access_person_entity_unique UNIQUE (person_id, entity_id)
);

ALTER TABLE atlas_safety_dashboard.entity_access OWNER TO atlas_safety_dashboard_user;

-- Backs findAllByPersonId / findAllByPersonIds, the read path on every profile and login.
CREATE INDEX IF NOT EXISTS entity_access_person_id_idx ON atlas_safety_dashboard.entity_access (person_id);

-- Backs "who manages this depot"; also the lookup a depot deletion has to check.
CREATE INDEX IF NOT EXISTS entity_access_entity_id_idx ON atlas_safety_dashboard.entity_access (entity_id);
