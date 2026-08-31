-- Backfill person.merchant_id (added in ddl-migrations/provider-dashboard/0097) from existing
-- access rows, so persons created before the column still have a durable owner for the
-- cross-merchant admin guards.
--
-- A person with access to several merchants gets the lowest merchant_id; the guard unions this
-- column with the access rows, so the other merchants keep their authority either way.
-- Idempotent: only fills rows that are still NULL, so persons provisioned after the deploy
-- (which set the column at creation) are never rewritten.
UPDATE atlas_dashboard.person p
SET merchant_id = sub.merchant_id
FROM (
  SELECT person_id, MIN(merchant_id) AS merchant_id
  FROM atlas_dashboard.merchant_access
  GROUP BY person_id
) sub
WHERE p.id = sub.person_id
  AND p.merchant_id IS NULL;
