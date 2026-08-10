-- ============================================================================
-- Phase 1 / 0005: merge persons.
-- Rules (PLAN.md decisions 2026-08-05):
--   * BPP row is canonical for both-sides persons (matched on email_hash);
--     BPP password_hash wins — no reset campaign.
--   * BAP-only persons copy over with their BAP ids.
--   * INTERNAL_ADMIN members become JUSPAY_ADMIN.
--   * Split-role deltas are handled later via person_capability GRANTs (0010).
--   * registration_token is NOT copied: everyone re-logs-in at cutover.
-- ============================================================================

-- 3.1 All BPP persons (canonical ids), with the INTERNAL_ADMIN remap.
INSERT INTO atlas_dashboard.person
  (id, first_name, last_name, role_id, email_encrypted, email_hash,
   mobile_number_encrypted, mobile_number_hash, mobile_country_code,
   password_hash, dashboard_access_type, dashboard_type, receive_notification,
   verified, created_at, updated_at, rejection_reason, rejected_at,
   password_updated_at, approved_by, rejected_by, language, secret_key,
   is2fa_enabled, token_no_hash, entity_id, admin_tier)
SELECT p.id, p.first_name, p.last_name,
       CASE WHEN r.name = 'INTERNAL_ADMIN'
            THEN (SELECT id FROM atlas_dashboard.role WHERE name = 'JUSPAY_ADMIN')
            ELSE p.role_id END,
       p.email_encrypted, p.email_hash,
       p.mobile_number_encrypted, p.mobile_number_hash, p.mobile_country_code,
       p.password_hash, p.dashboard_access_type, p.dashboard_type, p.receive_notification,
       p.verified, p.created_at, p.updated_at, p.rejection_reason, p.rejected_at,
       p.password_updated_at, p.approved_by, p.rejected_by, p.language, p.secret_key,
       p.is2fa_enabled, p.token_no_hash, p.entity_id, p.admin_tier
FROM atlas_dashboard.bpp_person p
JOIN atlas_dashboard.bpp_role r ON r.id = p.role_id;

-- 3.2 Mapping table for both-sides persons (BAP id -> canonical BPP id).
INSERT INTO atlas_dashboard.legacy_bap_person (bap_person_id, person_id, email_hash)
SELECT a.id, b.id, a.email_hash
FROM atlas_dashboard.bap_person a
JOIN atlas_dashboard.bpp_person b ON b.email_hash = a.email_hash
WHERE a.email_hash IS NOT NULL;

-- 3.2b Mobile fallback. The merged `person` has a UNIQUE
-- (mobile_country_code, mobile_number_hash), so a person present on both sides
-- under a DIFFERENT email would otherwise be inserted twice and violate it.
-- Reviewed 2026-08-09: every such pair in prod is the same human with a stale
-- email on one side, so the phone is treated as an identity key on its own.
INSERT INTO atlas_dashboard.legacy_bap_person (bap_person_id, person_id, email_hash)
SELECT DISTINCT ON (a.id) a.id, b.id, a.email_hash
FROM atlas_dashboard.bap_person a
JOIN atlas_dashboard.bpp_person b
  ON b.mobile_number_hash = a.mobile_number_hash
 AND b.mobile_country_code = a.mobile_country_code
WHERE a.id NOT IN (SELECT bap_person_id FROM atlas_dashboard.legacy_bap_person)
ORDER BY a.id, b.updated_at DESC NULLS LAST
ON CONFLICT DO NOTHING;

-- 3.2c Where the two rows disagree and the BAP row is NEWER, refresh the
-- surviving person's profile from it (the BPP id stays canonical — everything
-- else references it). Password, 2FA and admin_tier are deliberately NOT
-- touched: the BPP password wins by an earlier decision (no reset campaign),
-- and changing credentials here would lock people out.
-- The email guard prevents stealing an address already held by someone else.
UPDATE atlas_dashboard.person p
SET first_name      = a.first_name,
    last_name       = a.last_name,
    email_encrypted = CASE WHEN ok.usable THEN a.email_encrypted ELSE p.email_encrypted END,
    email_hash      = CASE WHEN ok.usable THEN a.email_hash      ELSE p.email_hash      END,
    updated_at      = a.updated_at
FROM atlas_dashboard.legacy_bap_person map
JOIN atlas_dashboard.bap_person a ON a.id = map.bap_person_id
JOIN atlas_dashboard.bpp_person b ON b.id = map.person_id
CROSS JOIN LATERAL (
  SELECT a.email_hash IS NOT NULL
     AND NOT EXISTS (SELECT 1 FROM atlas_dashboard.person q
                     WHERE q.email_hash = a.email_hash AND q.id <> map.person_id) AS usable
) ok
WHERE p.id = map.person_id
  AND a.updated_at > b.updated_at
  AND a.email_hash IS DISTINCT FROM b.email_hash;

-- 3.2d Report: pairs merged on phone alone (emails differed), and which side
-- won. Spot-check a few before continuing.
SELECT map.bap_person_id, map.person_id AS surviving_person_id,
       p.first_name, p.last_name,
       CASE WHEN a.updated_at > b.updated_at THEN 'BAP (newer)' ELSE 'BPP' END AS profile_from,
       a.updated_at AS bap_updated, b.updated_at AS bpp_updated
FROM atlas_dashboard.legacy_bap_person map
JOIN atlas_dashboard.bap_person a ON a.id = map.bap_person_id
JOIN atlas_dashboard.bpp_person b ON b.id = map.person_id
JOIN atlas_dashboard.person p     ON p.id = map.person_id
WHERE a.email_hash IS DISTINCT FROM b.email_hash
ORDER BY p.first_name;

-- 3.3 BAP-only persons: not matched by email (3.2) or phone (3.2b). Role
-- remapped by NAME into the merged role set. DISTINCT ON keeps one row per phone should the BAP side
-- itself contain two (it has no such unique constraint), newest wins.
INSERT INTO atlas_dashboard.person
  (id, first_name, last_name, role_id, email_encrypted, email_hash,
   mobile_number_encrypted, mobile_number_hash, mobile_country_code,
   password_hash, dashboard_access_type, dashboard_type, receive_notification,
   verified, created_at, updated_at, rejection_reason, rejected_at,
   password_updated_at, approved_by, rejected_by, language, secret_key,
   is2fa_enabled, token_no_hash, entity_id, admin_tier)
SELECT a.id, a.first_name, a.last_name,
       rd.id,
       a.email_encrypted, a.email_hash,
       a.mobile_number_encrypted, a.mobile_number_hash, a.mobile_country_code,
       a.password_hash, a.dashboard_access_type, a.dashboard_type, a.receive_notification,
       a.verified, a.created_at, a.updated_at, a.rejection_reason, a.rejected_at,
       a.password_updated_at, a.approved_by, a.rejected_by, a.language, a.secret_key,
       a.is2fa_enabled, a.token_no_hash, a.entity_id, a.admin_tier
FROM (
  SELECT DISTINCT ON (mobile_country_code, mobile_number_hash) *
  FROM atlas_dashboard.bap_person
  ORDER BY mobile_country_code, mobile_number_hash, updated_at DESC NULLS LAST
) a
JOIN atlas_dashboard.bap_role ra ON ra.id = a.role_id
JOIN atlas_dashboard.role rd ON rd.name = ra.name
WHERE a.id NOT IN (SELECT bap_person_id FROM atlas_dashboard.legacy_bap_person)
  AND NOT EXISTS (
    SELECT 1 FROM atlas_dashboard.person p
    WHERE p.mobile_number_hash = a.mobile_number_hash
      AND p.mobile_country_code = a.mobile_country_code);

-- 3.4 Guard: every BAP person is mapped (3.2/3.2b) or copied (3.3). The only
-- expected rows are intra-BAP phone duplicates dropped by 3.3's DISTINCT ON;
-- anything else here is a bug.
SELECT count(*) AS bap_persons_lost
FROM atlas_dashboard.bap_person a
WHERE a.id NOT IN (SELECT bap_person_id FROM atlas_dashboard.legacy_bap_person)
  AND a.id NOT IN (SELECT id FROM atlas_dashboard.person);
-- If > 0: those persons' roles were dropped by 02 (zero-member rule) while
-- they still exist — inspect with preflight 0.6 (0001) before proceeding.

-- 3.5a A person may have been promoted on the BAP side only; take the more
-- privileged tier of the two (BPP row is canonical for everything else).
UPDATE atlas_dashboard.person p
SET admin_tier = 'SUPER_ADMIN'
FROM atlas_dashboard.legacy_bap_person map
JOIN atlas_dashboard.bap_person a ON a.id = map.bap_person_id
WHERE p.id = map.person_id AND a.admin_tier = 'SUPER_ADMIN' AND p.admin_tier <> 'SUPER_ADMIN';

-- 3.5 Initialize admin_tier from the merged role's coarse tier.
-- SUPER_ADMIN is never CREATED here — it is seeded manually via a separate,
-- vault-controlled DB entry (PLAN.md admin tiering) — but an existing one is
-- carried across (3.1/3.3 copy the column, 3.5a reconciles the two sides).
UPDATE atlas_dashboard.person p
SET admin_tier = 'DASHBOARD_ADMIN'
FROM atlas_dashboard.role r
WHERE r.id = p.role_id AND r.dashboard_access_type = 'DASHBOARD_ADMIN'
  AND p.admin_tier <> 'SUPER_ADMIN';  -- never downgrade a seeded super admin
