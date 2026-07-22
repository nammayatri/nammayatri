-- Seed for the PhoneShareConsentFlow integration suite.
--
-- The consent gate is an AND: the merchant's driver_calling_option must be
-- DirectCall/DualCall AND the rider must have consented. The local default is
-- 'AnonymousCall' (see migrations-read-only/dynamic-offer-driver-app/
-- transporter_config.sql:481), under which the rider's number is never shared
-- regardless of consent — so the suite could not exercise the consent half at
-- all without this seed.
--
-- Applied to every city so the suite stays city-agnostic (collections must not
-- contain city-specific logic). Local test DB only; harmless to other suites —
-- none assert on riderMobileNumber being absent.
--
-- The merchant-kill-switch half of the gate (AnonymousCall + consent => masked)
-- is covered by unit tests (test/Domain/Action/UI/RideSpec.hs, the 3x2 matrix)
-- since toggling transporter_config mid-collection would require a cache flush
-- between Newman steps.
--
-- NOTE: transporter_config is cached (ConfigPilot/Hedis). run-tests.sh flushes
-- Redis after applying this seed; if running manually, flush Redis yourself or
-- the running driver-app will keep serving the cached AnonymousCall value.

UPDATE atlas_driver_offer_bpp.transporter_config
SET driver_calling_option = 'DirectCall';
