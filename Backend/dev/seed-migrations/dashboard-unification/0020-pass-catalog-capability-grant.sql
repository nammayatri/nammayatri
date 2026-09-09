-- Grant the pass catalog capability to whoever already has the pass catalog.
--
-- Adding `capability:` to the Pass spec moved four endpoints that shipped
-- earlier (LIST_PASS_CATALOG, CREATE_PASS, UPDATE_PASS, DELETE_PASS) onto
-- city-config.pass_catalog.*. They had no capability_endpoint rows before, so
-- they resolved purely through the legacy access matrix via the
-- `capability OR legacy` fallback in Tools.Auth.Api.verifyAccessLevel.
--
-- That fallback still carries them today, which is why nothing is broken yet.
-- But the capability is granted to nobody, so the day the fallback is removed
-- every holder 403s. This closes that gap the way 0017 did: derive the grant
-- from what the matrix already allows, rather than naming roles that differ
-- per environment.
--
-- Read and write are granted together deliberately. The matrix only ever
-- covered the four original endpoints and only with USER_FULL_ACCESS, so a
-- role that could author the catalog before can author it now — no widening
-- beyond what it already had.
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT DISTINCT am.role_id, c.cap
FROM atlas_dashboard.access_matrix am
CROSS JOIN (VALUES ('city-config.pass_catalog.read'),
                   ('city-config.pass_catalog.write')) AS c(cap)
WHERE am.user_action_type IN (
        'RIDER_APP_MANAGEMENT/PASS/LIST_PASS_CATALOG',
        'RIDER_APP_MANAGEMENT/PASS/CREATE_PASS',
        'RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS',
        'RIDER_APP_MANAGEMENT/PASS/DELETE_PASS')
  AND am.user_access_type = 'USER_FULL_ACCESS'
ON CONFLICT DO NOTHING;
