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
   is2fa_enabled, token_no_hash, entity_id)
SELECT p.id, p.first_name, p.last_name,
       CASE WHEN r.name = 'INTERNAL_ADMIN'
            THEN (SELECT id FROM atlas_dashboard.role WHERE name = 'JUSPAY_ADMIN')
            ELSE p.role_id END,
       p.email_encrypted, p.email_hash,
       p.mobile_number_encrypted, p.mobile_number_hash, p.mobile_country_code,
       p.password_hash, p.dashboard_access_type, p.dashboard_type, p.receive_notification,
       p.verified, p.created_at, p.updated_at, p.rejection_reason, p.rejected_at,
       p.password_updated_at, p.approved_by, p.rejected_by, p.language, p.secret_key,
       p.is2fa_enabled, p.token_no_hash, p.entity_id
FROM atlas_bpp_dashboard.person p
JOIN atlas_bpp_dashboard.role r ON r.id = p.role_id;

-- 3.2 Mapping table for both-sides persons (BAP id -> canonical BPP id).
INSERT INTO atlas_dashboard.legacy_bap_person (bap_person_id, person_id, email_hash)
SELECT a.id, b.id, a.email_hash
FROM atlas_bap_dashboard.person a
JOIN atlas_bpp_dashboard.person b ON b.email_hash = a.email_hash
WHERE a.email_hash IS NOT NULL;

-- 3.3 BAP-only persons (includes NULL-email BAP rows: phase 0 found zero
-- cross-side mobile matches, so they are genuinely BAP-only). Role remapped
-- by NAME into the merged role set.
INSERT INTO atlas_dashboard.person
  (id, first_name, last_name, role_id, email_encrypted, email_hash,
   mobile_number_encrypted, mobile_number_hash, mobile_country_code,
   password_hash, dashboard_access_type, dashboard_type, receive_notification,
   verified, created_at, updated_at, rejection_reason, rejected_at,
   password_updated_at, approved_by, rejected_by, language, secret_key,
   is2fa_enabled, token_no_hash, entity_id)
SELECT a.id, a.first_name, a.last_name,
       rd.id,
       a.email_encrypted, a.email_hash,
       a.mobile_number_encrypted, a.mobile_number_hash, a.mobile_country_code,
       a.password_hash, a.dashboard_access_type, a.dashboard_type, a.receive_notification,
       a.verified, a.created_at, a.updated_at, a.rejection_reason, a.rejected_at,
       a.password_updated_at, a.approved_by, a.rejected_by, a.language, a.secret_key,
       a.is2fa_enabled, a.token_no_hash, a.entity_id
FROM atlas_bap_dashboard.person a
JOIN atlas_bap_dashboard.role ra ON ra.id = a.role_id
JOIN atlas_dashboard.role rd ON rd.name = ra.name
WHERE a.id NOT IN (SELECT bap_person_id FROM atlas_dashboard.legacy_bap_person);

-- 3.4 Guard: every BAP person is either mapped or copied. MUST return 0.
SELECT count(*) AS bap_persons_lost
FROM atlas_bap_dashboard.person a
WHERE a.id NOT IN (SELECT bap_person_id FROM atlas_dashboard.legacy_bap_person)
  AND a.id NOT IN (SELECT id FROM atlas_dashboard.person);
-- If > 0: those persons' roles were dropped by 02 (zero-member rule) while
-- they still exist — inspect with preflight 0.6 (0001) before proceeding.

-- 3.5 Initialize admin_tier from the merged role's coarse tier.
-- SUPER_ADMIN is NOT set here — it is seeded manually via a separate,
-- vault-controlled DB entry (PLAN.md admin tiering).
UPDATE atlas_dashboard.person p
SET admin_tier = 'DASHBOARD_ADMIN'
FROM atlas_dashboard.role r
WHERE r.id = p.role_id AND r.dashboard_access_type = 'DASHBOARD_ADMIN';
