-- Per-city gate for the SharedCabReconciler job; Nothing/false ends the job's own chain (fail-closed), so cities that
-- never opted in make no BAP session checks.
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN IF NOT EXISTS shared_cab_reconciler_enabled boolean DEFAULT false;
