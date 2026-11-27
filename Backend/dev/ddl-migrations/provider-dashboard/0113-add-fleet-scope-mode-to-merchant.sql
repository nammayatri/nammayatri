-- Per-merchant switch for scoped role assignments: OFF (ignored, the default), SHADOW (compared
-- with the old answer and logged) or ENFORCE (applied). Read by the dashboard permission check.
ALTER TABLE atlas_dashboard.merchant
  ADD COLUMN IF NOT EXISTS fleet_scope_mode character varying(16) NOT NULL DEFAULT 'OFF';
