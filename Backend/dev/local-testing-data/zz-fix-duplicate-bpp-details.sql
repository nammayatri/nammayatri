-- Runs after config-sync (files here are applied in name order by
-- test-tool/context-api/server.py _apply_local_testing_data), before
-- rider-app / driver-app are restarted.
--
-- config-sync rewrites several master driver-backend hosts (e.g.
-- api.sandbox.beckn.juspay.in/dev/dobpp and api.sandbox.moving.tech/dev/dobpp)
-- to the same local host, so the synced atlas_app.bpp_details can hold the
-- same (subscriber_id, domain) more than once. The lookup (findOneWithKV)
-- expects one row and returns nothing for duplicates once the Redis cache
-- expires: the rider's flowStatus fails with "BppDetails not found"
-- ("Unable to fetch the latest ride info"), and the rider app inserts yet
-- another copy. Keep only the newest row per (subscriber_id, domain).
DELETE FROM atlas_app.bpp_details d
USING atlas_app.bpp_details newer
WHERE d.subscriber_id = newer.subscriber_id
  AND d.domain = newer.domain
  AND (d.created_at, d.id) < (newer.created_at, newer.id);
