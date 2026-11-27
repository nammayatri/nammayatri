-- Fleet scope: where person_role_assignment has drifted from the person's role and city access.
--
-- Every app-level writer keeps the table in step (Domain.Action.Dashboard.AssignmentSync). Seeds,
-- generate-admin-hashes.hs and config-sync write role and merchant_access directly, so this is how
-- to find what they left behind. Read-only; run it against atlas_dashboard. Each result should be
-- empty before a merchant moves from SHADOW to ENFORCE.

-- 1. A person has city access in a merchant but no ACTIVE city assignment for it (fleet owners and
--    operators are left out: their access is not city based).
SELECT p.id AS person_id, ma.merchant_id, ma.operating_city, r.name AS role
  FROM atlas_dashboard.merchant_access ma
  JOIN atlas_dashboard.person p ON p.id = ma.person_id
  JOIN atlas_dashboard.role r ON r.id = p.role_id
 WHERE r.dashboard_access_type NOT IN ('FLEET_OWNER', 'RENTAL_FLEET_OWNER', 'DASHBOARD_OPERATOR')
   AND NOT EXISTS (
         SELECT 1 FROM atlas_dashboard.person_role_assignment a
          WHERE a.person_id = ma.person_id
            AND a.role_id = p.role_id
            AND a.scope_type = 'CITY'
            AND a.scope_id = ma.operating_city
            AND a.merchant_id = ma.merchant_id
            AND a.status = 'ACTIVE');

-- 2. An ACTIVE city assignment whose city access, or role, the person no longer has.
SELECT a.id AS assignment_id, a.person_id, a.merchant_id, a.scope_id AS city
  FROM atlas_dashboard.person_role_assignment a
  JOIN atlas_dashboard.person p ON p.id = a.person_id
 WHERE a.scope_type = 'CITY'
   AND a.status = 'ACTIVE'
   AND (a.role_id <> p.role_id
        OR NOT EXISTS (
             SELECT 1 FROM atlas_dashboard.merchant_access ma
              WHERE ma.person_id = a.person_id
                AND ma.merchant_id = a.merchant_id
                AND ma.operating_city = a.scope_id));

-- 3. A fleet owner with no ACTIVE assignment on their own fleet.
SELECT p.id AS fleet_owner_id, p.merchant_id
  FROM atlas_dashboard.person p
  JOIN atlas_dashboard.role r ON r.id = p.role_id
 WHERE r.dashboard_access_type IN ('FLEET_OWNER', 'RENTAL_FLEET_OWNER')
   AND p.merchant_id IS NOT NULL
   AND NOT EXISTS (
         SELECT 1 FROM atlas_dashboard.person_role_assignment a
          WHERE a.person_id = p.id
            AND a.scope_type = 'FLEET'
            AND a.scope_id = p.id
            AND a.status = 'ACTIVE');

-- 4. A fleet left with no ACTIVE admin at all.
SELECT scope_id AS fleet_id, merchant_id
  FROM atlas_dashboard.person_role_assignment
 WHERE scope_type = 'FLEET'
 GROUP BY scope_id, merchant_id
HAVING bool_and(status <> 'ACTIVE');
