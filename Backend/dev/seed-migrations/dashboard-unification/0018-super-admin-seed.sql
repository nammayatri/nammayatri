\set ON_ERROR_STOP on

-- ===========================================================================
-- 0017 — seed the initial SUPER_ADMIN(s).
--
-- SUPER_ADMIN is deliberately not mintable over the API: there is no handler
-- that writes this tier, by design (PLAN.md admin tiering). This file is the
-- only way in, which is why it is run by hand with vault-held credentials
-- rather than folded into the automated runbook.
--
-- WHY IT IS NOW REQUIRED, not optional. guardAdminMutation used to be
-- existence-guarded — the SUPER_ADMIN rule stayed dormant until a SUPER_ADMIN
-- row existed, so DASHBOARD_ADMINs kept minting admins. That guard is gone.
-- With no SUPER_ADMIN seeded, NOBODY can create or modify a DASHBOARD_ADMIN
-- or DASHBOARD_RELEASE_ADMIN. Run this in the same window as the deploy.
--
-- SUPER_ADMIN also bypasses the capability check entirely (break-glass), and
-- every such request logs SUPER_ADMIN_BREAKGLASS. Grant it to as few people as
-- possible — two is the usual answer, so one person being unavailable is not
-- an outage.
-- ===========================================================================

-- ---------------------------------------------------------------------------
-- 0. Find the person ids. email/mobile are encrypted and their _hash columns
--    are deterministic HMACs, not plaintext — you cannot match on a typed-in
--    email here. Names are plaintext, so look people up by name and copy the
--    id. Run this first, on its own:
--
--    SELECT p.id, p.first_name, p.last_name, r.name AS role_name, p.admin_tier
--    FROM atlas_dashboard.person p
--    LEFT JOIN atlas_dashboard.role r ON r.id = p.role_id
--    WHERE p.first_name ILIKE '%<name>%' OR p.last_name ILIKE '%<name>%'
--    ORDER BY p.first_name;

-- ---------------------------------------------------------------------------
-- 1. EDIT THIS. The person ids from step 0.
CREATE TEMP TABLE super_admin_seed (person_id text PRIMARY KEY);
INSERT INTO super_admin_seed (person_id) VALUES
    ('CHANGE_ME')
    -- ,('<second break-glass person id>')
    ;

-- ---------------------------------------------------------------------------
-- 2. Refuse to run against placeholders or ids that do not exist.
DO $$
DECLARE
  unmatched text;
BEGIN
  IF EXISTS (SELECT 1 FROM super_admin_seed WHERE person_id = 'CHANGE_ME') THEN
    RAISE EXCEPTION 'Edit section 1 first — the placeholder id is still there.';
  END IF;

  SELECT string_agg(s.person_id, ', ') INTO unmatched
  FROM super_admin_seed s
  WHERE NOT EXISTS (
    SELECT 1 FROM atlas_dashboard.person p WHERE p.id = s.person_id);

  IF unmatched IS NOT NULL THEN
    RAISE EXCEPTION 'No person row for id: %. Re-check against step 0.', unmatched;
  END IF;
END $$;

-- ---------------------------------------------------------------------------
-- 3. Grant the tier.
UPDATE atlas_dashboard.person p
SET admin_tier = 'SUPER_ADMIN'
FROM super_admin_seed s
WHERE p.id = s.person_id;

-- ---------------------------------------------------------------------------
-- 4. Audit the grant like any other access mutation. actor_id is NULL because
--    this came from a migration, not a person acting through the UI.
INSERT INTO atlas_dashboard.access_audit
       (id, actor_id, action, target_type, target_id, before_value, after_value, reason, created_at)
SELECT md5('super-admin-seed:' || p.id)::uuid::text,
       NULL,
       'ADMIN_TIER_UPDATE',
       'person',
       p.id,
       'USER',
       'SUPER_ADMIN',
       'Initial break-glass seed (0017); SUPER_ADMIN is not mintable via API',
       now()
FROM atlas_dashboard.person p
JOIN super_admin_seed s ON s.person_id = p.id
ON CONFLICT DO NOTHING;

-- ---------------------------------------------------------------------------
-- 5. Confirm. Expect exactly the rows you listed in section 1.
SELECT p.id, p.first_name, p.last_name, p.admin_tier, r.name AS role_name
FROM atlas_dashboard.person p
LEFT JOIN atlas_dashboard.role r ON r.id = p.role_id
WHERE p.admin_tier = 'SUPER_ADMIN';

-- ---------------------------------------------------------------------------
-- To revoke (person leaves, or the break-glass window closes):
--   UPDATE atlas_dashboard.person SET admin_tier = 'USER' WHERE id = '...';
-- Then flush the auth cache so it takes effect inside 10 minutes:
--   redis-cli DEL "dsh:caps:v<N>:<personId>"   -- or bump dsh:caps:version
