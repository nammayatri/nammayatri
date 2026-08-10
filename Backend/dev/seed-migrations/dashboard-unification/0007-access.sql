-- ============================================================================
-- Phase 1 / 0007: merge merchant_access + access_matrix.
-- A matched person may gain the UNION of their BAP-side and BPP-side
-- merchant/city grants — intended behavior (PLAN.md).
-- ============================================================================

-- 5.1 merchant_access: BPP rows unchanged (person/merchant ids canonical).
INSERT INTO atlas_dashboard.merchant_access
  (id, person_id, merchant_id, merchant_short_id, operating_city, created_at)
SELECT id, person_id, merchant_id, merchant_short_id, operating_city, created_at
FROM atlas_dashboard.bpp_merchant_access;

-- 5.2 merchant_access: BAP rows, with BOTH ids remapped — person_id for
-- matched persons, merchant_id for merchants re-ided in 0006 (the two sides
-- reuse some merchant UUIDs). short_id is unchanged and disjoint across sides,
-- so (person, merchant, city) still cannot collide with 5.1.
-- merchant_access is UNIQUE (person_id, merchant_id, operating_city). Two BAP
-- persons can now map to ONE merged person (0005's phone fallback), so their
-- grants can collide with each other — and, for a matched person, with a row
-- already inserted from the BPP side. Deduplicate on the constraint key,
-- keeping the earliest grant, and skip anything already present.
INSERT INTO atlas_dashboard.merchant_access
  (id, person_id, merchant_id, merchant_short_id, operating_city, created_at)
SELECT DISTINCT ON (r.person_id, r.merchant_id, r.operating_city)
       r.id, r.person_id, r.merchant_id, r.merchant_short_id, r.operating_city, r.created_at
FROM (
  SELECT ma.id,
         coalesce(map.person_id, ma.person_id)     AS person_id,
         coalesce(mm.merchant_id, ma.merchant_id)  AS merchant_id,
         ma.merchant_short_id, ma.operating_city, ma.created_at
  FROM atlas_dashboard.bap_merchant_access ma
  LEFT JOIN atlas_dashboard.legacy_bap_person map ON map.bap_person_id = ma.person_id
  LEFT JOIN atlas_dashboard.legacy_bap_merchant mm ON mm.bap_merchant_id = ma.merchant_id
) r
-- Drop rows whose person didn't survive (retired-role members; preflight 0.6
-- (0001) says there are none, this is belt+braces):
WHERE r.person_id IN (SELECT id FROM atlas_dashboard.person)
  AND NOT EXISTS (
    SELECT 1 FROM atlas_dashboard.merchant_access x
    WHERE x.person_id = r.person_id
      AND x.merchant_id = r.merchant_id
      AND x.operating_city = r.operating_city)
ORDER BY r.person_id, r.merchant_id, r.operating_city, r.created_at;

-- Report: BAP grants collapsed by the dedupe above (two BAP persons merged into
-- one, or a grant the BPP side already had). Access is unchanged — the same
-- person keeps the same merchant/city — but the row count will not add up.
SELECT count(*) AS bap_grants_collapsed
FROM (
  SELECT coalesce(map.person_id, ma.person_id) AS person_id,
         coalesce(mm.merchant_id, ma.merchant_id) AS merchant_id,
         ma.operating_city
  FROM atlas_dashboard.bap_merchant_access ma
  LEFT JOIN atlas_dashboard.legacy_bap_person map ON map.bap_person_id = ma.person_id
  LEFT JOIN atlas_dashboard.legacy_bap_merchant mm ON mm.bap_merchant_id = ma.merchant_id
  WHERE coalesce(map.person_id, ma.person_id) IN (SELECT id FROM atlas_dashboard.person)
) r
GROUP BY r.person_id, r.merchant_id, r.operating_city
HAVING count(*) > 1;

-- 5.3 access_matrix: BPP rows unchanged where the role survived.
INSERT INTO atlas_dashboard.access_matrix
  (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at)
SELECT m.id, m.role_id, m.api_entity, m.user_access_type, m.user_action_type,
       m.created_at, m.updated_at
FROM atlas_dashboard.bpp_access_matrix m
WHERE m.role_id IN (SELECT id FROM atlas_dashboard.role);

-- 5.4 access_matrix: BAP rows, role remapped by NAME; skip exact duplicates
-- (same role/entity/action already present from the BPP side). This is what
-- makes a shared-name role's grant set the UNION of both sides — and via the
-- capability derivation, its bundle too.
INSERT INTO atlas_dashboard.access_matrix
  (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at)
SELECT m.id, rd.id, m.api_entity, m.user_access_type, m.user_action_type,
       m.created_at, m.updated_at
FROM atlas_dashboard.bap_access_matrix m
JOIN atlas_dashboard.bap_role ra ON ra.id = m.role_id
JOIN atlas_dashboard.role rd ON rd.name = ra.name
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_dashboard.access_matrix t
  WHERE t.role_id = rd.id
    AND t.api_entity = m.api_entity
    AND t.user_action_type = m.user_action_type);

-- 5.5 Purge stale grants: matrix rows pointing at endpoints that no longer
-- exist in code (the 13 renamed/deleted ids + 12 BHARAT_TAXI_* — see
-- capability-seed.md §3.5). Deliberately a DELETE on the MERGED schema only;
-- sources stay untouched.
DELETE FROM atlas_dashboard.access_matrix
WHERE user_action_type IN (
  'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_CHANGE_DRIVER',
  'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_FLEET_OPERATOR_CHANGE',
  'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_FLEET_OPERATOR_CREATE',
  'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_OPERATOR_CHANGE',
  'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_CONCLUDE_OR_ABORT_OR_REVERT',
  'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_SERVICE_CATEGORY_DEL_PEOPLE',
  'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_SERVICE_CATEGORY_UPDATE_PEOPLE',
  'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_CANCELLATION_DUES_SYNC',
  'RIDER_MANAGEMENT/FRFS_TICKET/POST_FRFS_TICKET_FRFS_ROUTE_ADD',
  'RIDER_MANAGEMENT/FRFS_TICKET/POST_FRFS_TICKET_FRFS_ROUTE_DELETE',
  'RIDER_MANAGEMENT/FRFS_TICKET/POST_FRFS_TICKET_FRFS_STATION_ADD',
  'RIDER_MANAGEMENT/FRFS_TICKET/POST_FRFS_TICKET_FRFS_STATION_DELETE',
  'RIDER_MANAGEMENT/INVOICE/GET_INVOICE_FINANCE_INVOICE_PDF')
   OR user_action_type LIKE 'BHARAT_TAXI_%';
