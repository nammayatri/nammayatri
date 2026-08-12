-- Allow one endpoint to be reachable through more than one capability.
--
-- 0095 keyed capability_endpoint on (server_name, endpoint_id), so an endpoint
-- resolved to exactly one capability. That made it impossible to give a narrow
-- role private access to an endpoint a broad role already holds: the row had to
-- MOVE, which took the endpoint away from everyone else. The concrete case is
-- the BOT reviewer flow, whose detail screens read driver/vehicle/document
-- endpoints that city-operations and fleet roles legitimately also use.
--
-- Widening the key to include capability_id makes the mapping a relation.
-- Tools.Auth.Capability.enforce now allows a request when the caller holds ANY
-- capability mapped to the endpoint, which is the same ANY-of rule the frontend
-- already applies to NavItem.requires.
--
-- The fail-closed property is unchanged: zero rows for an endpoint still denies
-- (CAPABILITY_UNMAPPED_ENDPOINT).
--
-- Operational note: this makes widening cheap and invisible. "Who can call this
-- endpoint" stops being a single lookup and becomes a union, so a review of
-- endpoints carrying more than one capability belongs in CI — see
-- docs/access-unification/PLAN.md.

ALTER TABLE atlas_dashboard.capability_endpoint
  DROP CONSTRAINT capability_endpoint_pkey;

ALTER TABLE atlas_dashboard.capability_endpoint
  ADD CONSTRAINT capability_endpoint_pkey
  PRIMARY KEY (server_name, endpoint_id, capability_id);
