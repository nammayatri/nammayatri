-- Seeds entity_access from the pre-existing single-depot person.entity_id, so depot managers
-- keep the depot they already had once the binary starts reading grants from the new table.
--
-- Idempotent and re-runnable: ON CONFLICT keeps a re-run from duplicating grants, and the join
-- to entity drops any dangling person.entity_id rather than seeding a grant to a depot that no
-- longer exists.
--
-- Run AFTER ddl-migrations/provider-dashboard/0106-create-entity-access.sql creates the table, and BEFORE the new
-- binary serves traffic -- until this runs, every existing depot manager reads as having none.

INSERT INTO atlas_dashboard.entity_access (id, person_id, entity_id, merchant_id, created_at)
SELECT md5(random()::text || p.id || e.id)::uuid::text, p.id, e.id, e.merchant_id, CURRENT_TIMESTAMP
FROM atlas_dashboard.person p
JOIN atlas_dashboard.entity e ON e.id = p.entity_id
WHERE p.entity_id IS NOT NULL
ON CONFLICT (person_id, entity_id) DO NOTHING;
