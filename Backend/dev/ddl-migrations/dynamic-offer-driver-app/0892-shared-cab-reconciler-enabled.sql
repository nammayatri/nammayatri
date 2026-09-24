-- Shared-cab reconciler city gate (NY shared-cab-prime, task 4.2B).
--
-- Per-city enablement for the SharedCabReconciler allocator job; Nothing/false
-- means the job terminates its own chain (fail-closed): the reconciler must
-- not run BAP session checks for cities that never opted in.
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN IF NOT EXISTS shared_cab_reconciler_enabled boolean DEFAULT false;
