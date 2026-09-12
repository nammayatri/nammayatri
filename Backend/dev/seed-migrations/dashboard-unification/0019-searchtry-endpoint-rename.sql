-- PROVIDER_MANAGEMENT/SEARCH_TRY: drop the endpoint id left behind by a path rename.
--
-- The spec's endpoint moved from /recent to /recentSearchTries, which changed the
-- generated id from POST_SEARCH_TRY_RECENT to POST_SEARCH_TRY_RECENT_SEARCH_TRIES.
-- The rename never reached capability_endpoint, so the live id had no row -- and
-- Capability.enforce fails closed, denying every call to this endpoint. That is
-- true on the proxied path today, not only after the direct-serving cutover.
--
-- The row for the new id is emitted by the generator (see
-- migrations-read-only/provider-dashboard/API_Management_SearchTry.sql) now that
-- the spec declares `capability: city-operations.ride.read` -- the same capability
-- the old id was granted, so this is a correction, not a permissions change.
--
-- All this migration does is remove the stale row: no endpoint emits the old id.

DELETE FROM atlas_dashboard.capability_endpoint
 WHERE endpoint_id = 'PROVIDER_MANAGEMENT/SEARCH_TRY/POST_SEARCH_TRY_RECENT';
