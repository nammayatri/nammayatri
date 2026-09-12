-- PROVIDER_MANAGEMENT/GEOHASH_AREA: drop endpoint ids that no endpoint emits.
--
-- provider-dashboard/0003-capability-seed.sql granted the two upsert endpoints as
-- GEOHASH_AREA_BULK_UPSERT and GEOHASH_AREA_CSV_UPSERT, but the ids the spec
-- generates are POST_GEOHASH_AREA_UPSERT and POST_GEOHASH_AREA_UPSERT_CSV. Those
-- rows are emitted by the generator (see
-- migrations-read-only/provider-dashboard/API_Management_GeohashArea.sql) under
-- the same city-config.geo.write capability, so this is a cleanup, not a
-- permissions change.
--
-- All this migration does is remove the stale rows.

DELETE FROM atlas_dashboard.capability_endpoint
 WHERE endpoint_id IN (
   'PROVIDER_MANAGEMENT/GEOHASH_AREA/GEOHASH_AREA_BULK_UPSERT',
   'PROVIDER_MANAGEMENT/GEOHASH_AREA/GEOHASH_AREA_CSV_UPSERT'
 );
