-- SWS-5 reminder copy for MSIL scheduled rides (Live - ONDC Ride flow doc, "Driver Readiness").
--
-- The go-online prompt now fires at MULTIPLE milestones (pickup -60/-45/-35 min), so its copy must
-- not hardcode a single "45 minutes", and it should surface the reallocation risk the driver can act on.
--   1. GO_ONLINE_REMINDER (PN)                  -> milestone-safe copy.
--   2. SMS_TO_GO_ONLINE_IN_SCHEDULED_RIDE (SMS) -> milestone-safe copy.
-- Idempotent (fixed-value UPDATEs). ENGLISH only for now (TODO: other languages once MSIL shares them).
-- The hourly text reminder is a separate new PN key, seeded in the companion config migration 0056.

-- 1. GO_ONLINE_REMINDER push copy: drop the hardcoded "45 minutes"; add the reallocation risk.
UPDATE atlas_driver_offer_bpp.merchant_push_notification mpn
SET title = 'Please Go Online',
    body = 'You have an upcoming assigned ride. Go online now and start moving to the pickup location.',
    updated_at = now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.id = mpn.merchant_operating_city_id
  AND moc.merchant_short_id = 'MSIL_PARTNER'
  AND mpn.key = 'GO_ONLINE_REMINDER';

-- 2. SMS_TO_GO_ONLINE_IN_SCHEDULED_RIDE SMS copy: same genericisation (keeps the {#driverPartnerName#} token).
UPDATE atlas_driver_offer_bpp.merchant_message mm
SET message = 'You have an upcoming assigned ride. Please go Online on {#driverPartnerName#} Partner app and start moving to the pickup location.',
    updated_at = now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.id = mm.merchant_operating_city_id
  AND moc.merchant_short_id = 'MSIL_PARTNER'
  AND mm.message_key = 'SMS_TO_GO_ONLINE_IN_SCHEDULED_RIDE';
