-- SWS-5 scheduled-ride reminder architecture for MSIL (Live-ONDC "Driver Readiness" i + iii).
-- Companion to 0055 (which genericised the existing go-online copy). Two distinct mechanisms:
--
--   (i)  Hourly reminder  -> NEW push key SCHEDULED_RIDE_UPCOMING_REMINDER. Anchored to RIDE_ASSIGNED,
--        recurs every hour (repeat_interval=3600) until pickup, for EVERYONE (only_if_offline=false).
--   (iii)Go-online prompts -> GO_ONLINE_REMINDER (push) at one-shot milestones pickup-60/-45/-35 min
--        (all BEFORE the pickup-30 activation, so the driver has time to react), only while offline.
--        SMS kept as a single -45 offline backup.
--
-- The existing SCHEDULED_RIDE_REMINDER (notification_type = CALL) config is left untouched — no deletes.
-- This migration is INSERT-only: it ADDS the hourly push key and the extra go-online push milestones.
-- The go-online push/SMS prompts stay one-shot because repeat_interval is a new column that defaults
-- NULL and nothing sets it on those keys; only the new hourly key carries a cadence (repeat_interval=3600).
--
-- Idempotent (NOT EXISTS guards). Clones merchant/city/on_booking_status from the existing
-- GO_ONLINE_REMINDER rows. NOTE: UNTESTED — DB was down when written; verify against live tables.

-- 1a. Hourly reminder CONFIG: new push key, recurring from ride-assignment to pickup, for everyone.
INSERT INTO atlas_driver_offer_bpp.ride_related_notification_config
  (id, merchant_id, merchant_operating_city_id, notification_key, notification_type,
   only_if_offline, time_diff, time_diff_event, on_booking_status, on_scheduled_booking, event_time, repeat_interval, created_at, updated_at)
SELECT DISTINCT ON (rrnc.merchant_operating_city_id)
  md5(rrnc.merchant_operating_city_id::text || '-SCHEDULED_RIDE_UPCOMING_REMINDER'),
  rrnc.merchant_id, rrnc.merchant_operating_city_id, 'SCHEDULED_RIDE_UPCOMING_REMINDER', 'PN',
  false, 0, 'RIDE_ASSIGNED', rrnc.on_booking_status, true, 'PostEvent', 3600, now(), now()
FROM atlas_driver_offer_bpp.ride_related_notification_config rrnc
JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON moc.id = rrnc.merchant_operating_city_id
WHERE rrnc.notification_key = 'GO_ONLINE_REMINDER' AND moc.merchant_short_id = 'MSIL_PARTNER'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.ride_related_notification_config x
    WHERE x.merchant_operating_city_id = rrnc.merchant_operating_city_id
      AND x.notification_key = 'SCHEDULED_RIDE_UPCOMING_REMINDER');

-- 1b. Hourly reminder COPY (the MSIL message).
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification
  (id, key, title, body, language, fcm_notification_type, fcm_sub_category, merchant_id, merchant_operating_city_id, trip_category, created_at, updated_at)
SELECT
  md5(mpn.merchant_operating_city_id::text || '-SCHEDULED_RIDE_UPCOMING_REMINDER-' || mpn.language),
  'SCHEDULED_RIDE_UPCOMING_REMINDER', 'You have an upcoming ride',
  'Please reach pick up point on time to avoid risk of cancellation.',
  mpn.language, mpn.fcm_notification_type, mpn.fcm_sub_category, mpn.merchant_id, mpn.merchant_operating_city_id, mpn.trip_category, now(), now()
FROM atlas_driver_offer_bpp.merchant_push_notification mpn
JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON moc.id = mpn.merchant_operating_city_id
WHERE mpn.key = 'GO_ONLINE_REMINDER' AND moc.merchant_short_id = 'MSIL_PARTNER' AND mpn.language = 'ENGLISH'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.merchant_push_notification x
    WHERE x.merchant_operating_city_id = mpn.merchant_operating_city_id
      AND x.key = 'SCHEDULED_RIDE_UPCOMING_REMINDER' AND x.language = mpn.language);

-- 2. Add the -60min (3600s) and -35min (2100s) push milestones (the -45min/2700s row already exists).
--    (one-shot: repeat_interval left NULL — nothing sets a cadence on GO_ONLINE_REMINDER).
INSERT INTO atlas_driver_offer_bpp.ride_related_notification_config
  (id, merchant_id, merchant_operating_city_id, notification_key, notification_type,
   only_if_offline, time_diff, time_diff_event, on_booking_status, on_scheduled_booking, event_time, repeat_interval, created_at, updated_at)
SELECT
  md5(rrnc.merchant_operating_city_id::text || '-GO_ONLINE_REMINDER-' || m.td::text),
  rrnc.merchant_id, rrnc.merchant_operating_city_id, 'GO_ONLINE_REMINDER', 'PN',
  true, m.td, 'PICKUP_TIME', rrnc.on_booking_status, true, 'PreEvent', NULL, now(), now()
FROM atlas_driver_offer_bpp.ride_related_notification_config rrnc
JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON moc.id = rrnc.merchant_operating_city_id
CROSS JOIN (VALUES (3600), (2100)) AS m(td)
WHERE rrnc.notification_key = 'GO_ONLINE_REMINDER' AND rrnc.time_diff = 2700
  AND moc.merchant_short_id = 'MSIL_PARTNER'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.ride_related_notification_config x
    WHERE x.merchant_operating_city_id = rrnc.merchant_operating_city_id
      AND x.notification_key = 'GO_ONLINE_REMINDER' AND x.time_diff = m.td);
