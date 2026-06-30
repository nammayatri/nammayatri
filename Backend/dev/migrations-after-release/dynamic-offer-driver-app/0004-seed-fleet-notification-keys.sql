-- Seed fleet notification keys for MSIL_PARTNER (all cities)
-- Other merchants can be added via the dashboard CRUD API

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification
  (id, key, title, body, language, fcm_notification_type, trip_category, fcm_sub_category, merchant_id, merchant_operating_city_id, should_trigger, channels, is_critical, created_at, updated_at)
SELECT
  md5(random()::text || clock_timestamp()::text)::uuid,
  'FLEET_DRIVER_LINKED',
  'Fleet Association',
  'You have been added to {#fleetName#}''s fleet',
  'ENGLISH',
  'DRIVER_NOTIFY',
  NULL,
  NULL,
  m.merchant_id,
  m.id,
  true,
  ARRAY['FCM', 'GRPC'],
  false,
  now(),
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city m
WHERE m.merchant_short_id = 'MSIL_PARTNER'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.merchant_push_notification
    WHERE key = 'FLEET_DRIVER_LINKED' AND merchant_operating_city_id = m.id AND language = 'ENGLISH'
  );

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification
  (id, key, title, body, language, fcm_notification_type, trip_category, fcm_sub_category, merchant_id, merchant_operating_city_id, should_trigger, channels, is_critical, created_at, updated_at)
SELECT
  md5(random()::text || clock_timestamp()::text)::uuid,
  'FLEET_DRIVER_REMOVED',
  'Fleet Association Ended',
  'You have been removed from {#fleetName#}''s fleet',
  'ENGLISH',
  'DRIVER_NOTIFY',
  NULL,
  NULL,
  m.merchant_id,
  m.id,
  true,
  ARRAY['FCM', 'GRPC'],
  false,
  now(),
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city m
WHERE m.merchant_short_id = 'MSIL_PARTNER'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.merchant_push_notification
    WHERE key = 'FLEET_DRIVER_REMOVED' AND merchant_operating_city_id = m.id AND language = 'ENGLISH'
  );

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification
  (id, key, title, body, language, fcm_notification_type, trip_category, fcm_sub_category, merchant_id, merchant_operating_city_id, should_trigger, channels, is_critical, created_at, updated_at)
SELECT
  md5(random()::text || clock_timestamp()::text)::uuid,
  'FLEET_VEHICLE_LINKED',
  'Vehicle Linked',
  'Vehicle {#vehicleNo#} has been linked to your account by {#fleetName#}',
  'ENGLISH',
  'DRIVER_NOTIFY',
  NULL,
  NULL,
  m.merchant_id,
  m.id,
  true,
  ARRAY['FCM', 'GRPC'],
  false,
  now(),
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city m
WHERE m.merchant_short_id = 'MSIL_PARTNER'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.merchant_push_notification
    WHERE key = 'FLEET_VEHICLE_LINKED' AND merchant_operating_city_id = m.id AND language = 'ENGLISH'
  );

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification
  (id, key, title, body, language, fcm_notification_type, trip_category, fcm_sub_category, merchant_id, merchant_operating_city_id, should_trigger, channels, is_critical, created_at, updated_at)
SELECT
  md5(random()::text || clock_timestamp()::text)::uuid,
  'FLEET_VEHICLE_UNLINKED',
  'Vehicle Unlinked',
  'Vehicle {#vehicleNo#} has been unlinked from your account',
  'ENGLISH',
  'DRIVER_NOTIFY',
  NULL,
  NULL,
  m.merchant_id,
  m.id,
  true,
  ARRAY['FCM'],
  false,
  now(),
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city m
WHERE m.merchant_short_id = 'MSIL_PARTNER'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.merchant_push_notification
    WHERE key = 'FLEET_VEHICLE_UNLINKED' AND merchant_operating_city_id = m.id AND language = 'ENGLISH'
  );
