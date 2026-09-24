-- Shared-cab taxi-pool exclusion flag (NY shared-cab-prime, task 4.2).
--
-- True on a driver's driver_information row means the driver is committed to a
-- shared-cab session and MUST NOT be dispatched plain taxi search requests:
-- excluded at pool fetch (GetNearestDrivers.buildDriverResult), skipped by the
-- SILENT direct-assign recheck, and rejected at accept time via the DB guard.
--
-- Fail-closed contract: session start writes True BEFORE pooled trips are
-- accepted on the session; only an explicit session-end/reconcile path writes
-- False. All existing drivers are not in a session, so the column defaults false.

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN shared_cab_session_active boolean NOT NULL DEFAULT false;
