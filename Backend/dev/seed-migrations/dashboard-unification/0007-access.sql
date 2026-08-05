-- ============================================================================
-- Phase 1 / 0007: merge merchant_access + access_matrix.
-- A matched person may gain the UNION of their BAP-side and BPP-side
-- merchant/city grants — intended behavior (PLAN.md).
-- ============================================================================

-- 5.1 merchant_access: BPP rows unchanged (person/merchant ids canonical).
INSERT INTO atlas_dashboard.merchant_access
  (id, person_id, merchant_id, merchant_short_id, operating_city, created_at)
SELECT id, person_id, merchant_id, merchant_short_id, operating_city, created_at
FROM atlas_bpp_dashboard.merchant_access;

-- 5.2 merchant_access: BAP rows, person_id remapped for matched persons.
-- BAP merchant ids exist verbatim in the merged merchant table, so no
-- merchant remap; (person, merchant, city) cannot collide with 5.1 because
-- the merchant sets are disjoint.
INSERT INTO atlas_dashboard.merchant_access
  (id, person_id, merchant_id, merchant_short_id, operating_city, created_at)
SELECT ma.id,
       coalesce(map.person_id, ma.person_id),
       ma.merchant_id, ma.merchant_short_id, ma.operating_city, ma.created_at
FROM atlas_bap_dashboard.merchant_access ma
LEFT JOIN atlas_dashboard.legacy_bap_person map ON map.bap_person_id = ma.person_id
-- Drop rows whose person didn't survive (retired-role members; preflight 0.6 (0001)
-- says there are none, this is belt+braces):
WHERE coalesce(map.person_id, ma.person_id) IN (SELECT id FROM atlas_dashboard.person);

-- 5.3 access_matrix: BPP rows unchanged where the role survived.
INSERT INTO atlas_dashboard.access_matrix
  (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at)
SELECT m.id, m.role_id, m.api_entity, m.user_access_type, m.user_action_type,
       m.created_at, m.updated_at
FROM atlas_bpp_dashboard.access_matrix m
WHERE m.role_id IN (SELECT id FROM atlas_dashboard.role);

-- 5.4 access_matrix: BAP rows, role remapped by NAME; skip exact duplicates
-- (same role/entity/action already present from the BPP side). This is what
-- makes a shared-name role's grant set the UNION of both sides — and via the
-- capability derivation, its bundle too.
INSERT INTO atlas_dashboard.access_matrix
  (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at)
SELECT m.id, rd.id, m.api_entity, m.user_access_type, m.user_action_type,
       m.created_at, m.updated_at
FROM atlas_bap_dashboard.access_matrix m
JOIN atlas_bap_dashboard.role ra ON ra.id = m.role_id
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
