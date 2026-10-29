-- Shared-cab taxi-pool exclusion flag: True means the driver is committed to a shared-cab session and MUST NOT be
-- dispatched plain taxi search requests. Fail-closed: session start writes True BEFORE pooled trips are accepted; only an
-- explicit session-end/reconcile path writes False. Existing drivers default to false.

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN IF NOT EXISTS shared_cab_session_active boolean NOT NULL DEFAULT false;
