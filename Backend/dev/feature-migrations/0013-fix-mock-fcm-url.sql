-- Point local FCM at the unified mock-server (port 8080, path /fcm) instead of the
-- standalone Haskell mock-fcm (4545). The Python mock-server ([Backend/dev/mock-servers])
-- serves FCM at /fcm (services/fcm.py) and is the mock the test-dashboard already proxies
-- (/proxy/mock-server), so overlays are readable from both the dashboard and curl. The
-- config-sync seed carries a legacy localhost FCM URL (e.g. http://localhost:4545/fcm);
-- this normalizes all local variants to http://localhost:8080/fcm.
-- Idempotent: rows already at 8080/fcm are skipped.

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
