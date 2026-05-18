-- Adds a nullable column for extension dashboards (control-center, etc.) to
-- store an action identifier that is opaque to NY. Populated only when
-- user_action_type = 'CONTROL_CENTER'; NULL for legacy rows.
--
-- The existing UNIQUE (role_id, api_entity, user_action_type) constraint is
-- replaced with two partial unique indexes so that:
--   * legacy rows (other_user_action_type IS NULL) keep their original
--     uniqueness on (role_id, api_entity, user_action_type), and
--   * multiple CONTROL_CENTER rows for the same role (different
--     other_user_action_type values) can coexist.
--
-- Partial unique indexes are used here for PostgreSQL 14 compatibility.
-- (UNIQUE NULLS NOT DISTINCT requires PostgreSQL 15+; the dev environment is
-- pinned to PG 14 — see Backend/dev/ddl-migrations/check_migrations.sh.)

ALTER TABLE atlas_bap_dashboard.access_matrix
    ADD COLUMN other_user_action_type varchar(255);

ALTER TABLE atlas_bap_dashboard.access_matrix
    DROP CONSTRAINT unique_role_id_api_entity_user_action_type;

CREATE UNIQUE INDEX unique_role_id_api_entity_user_action_type_null
    ON atlas_bap_dashboard.access_matrix (role_id, api_entity, user_action_type)
    WHERE other_user_action_type IS NULL;

CREATE UNIQUE INDEX unique_role_id_api_entity_user_action_type_other
    ON atlas_bap_dashboard.access_matrix (role_id, api_entity, user_action_type, other_user_action_type)
    WHERE other_user_action_type IS NOT NULL;
