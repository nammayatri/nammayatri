-- Mirror of provider-dashboard/0096 for the rider schema.
--
-- rider-dashboard is slated for deletion (PLAN.md Phase 7), but it still serves
-- prod BAP traffic and 0062 created the same capability tables here, so the key
-- has to widen on both sides or the two schemas disagree on what is insertable.
--
-- See provider-dashboard/0096-capability-endpoint-multi-capability.sql for the
-- rationale.

ALTER TABLE atlas_bap_dashboard.capability_endpoint
  DROP CONSTRAINT capability_endpoint_pkey;

ALTER TABLE atlas_bap_dashboard.capability_endpoint
  ADD CONSTRAINT capability_endpoint_pkey
  PRIMARY KEY (server_name, endpoint_id, capability_id);
