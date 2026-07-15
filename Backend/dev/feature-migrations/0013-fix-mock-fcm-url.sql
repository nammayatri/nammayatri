-- Repoint FCM URLs at the python mock-server (port 8080) instead of the deprecated
-- mock-fcm-exe (port 4545). The python mock at 8080 mounts /fcm/* and accepts any
-- subpath as a send; the old Haskell mock only accepted POST at root and returned
-- 404 for the Firebase-style /fcm/v1/projects/.../messages:send path, silently
-- dropping every driver FCM (e.g. the UPDATE_LOC_FCM overlay used by integration
-- tests to learn the BPP-side bookingUpdateRequestId).
--
-- BPP reads fcmConfig.fcmUrl from transporter_config via findFCMConfigWithFallback
-- (Tools.Notifications.hs:157). The merchant_service_config rows are updated too
-- for older / fallback code paths and BAP symmetry.
--
-- Idempotent: only touches localhost URLs that aren't already the desired value;
-- production URLs (https://fcm.googleapis.com/...) are untouched.
--
-- After running this, restart dynamic-offer-driver-app (or invalidate the
-- TransporterConfig cache in Redis) — BPP caches transporter_config in memory.

UPDATE atlas_driver_offer_bpp.transporter_config
SET fcm_url = 'http://localhost:8080/fcm'
WHERE fcm_url LIKE 'http://localhost:%'
  AND fcm_url <> 'http://localhost:8080/fcm';

UPDATE atlas_driver_offer_bpp.merchant_service_config
SET config_json = jsonb_set(config_json::jsonb, '{fcmUrl}', '"http://localhost:8080/fcm"')::json
WHERE service_name = 'Notification_FCM'
  AND config_json->>'fcmUrl' LIKE 'http://localhost:%'
  AND config_json->>'fcmUrl' <> 'http://localhost:8080/fcm';

UPDATE atlas_app.merchant_service_config
SET config_json = jsonb_set(config_json::jsonb, '{fcmUrl}', '"http://localhost:8080/fcm"')::json
WHERE service_name = 'Notification_FCM'
  AND config_json->>'fcmUrl' LIKE 'http://localhost:%'
  AND config_json->>'fcmUrl' <> 'http://localhost:8080/fcm';
