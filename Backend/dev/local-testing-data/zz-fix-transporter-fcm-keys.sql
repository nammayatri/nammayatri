-- Runs after config-sync (files here are applied in name order by
-- test-tool/context-api/server.py _apply_local_testing_data), before
-- rider-app / driver-app are restarted.
--
-- config-sync swaps the FCM service account in merchant_service_config
-- (Notification_FCM) for a fake one, but not in transporter_config, so the
-- synced snapshot carries master's real Google service accounts into every
-- local stack and devbox, and a few rows still send to fcm.googleapis.com.
-- Replace them with a fake value and point FCM at the mock, the same as
-- merchant_service_config. Driver ride requests still reach the app over gRPC.
UPDATE atlas_driver_offer_bpp.transporter_config
SET fcm_service_account = 'devbox-fake-fcm-service-account',
    fcm_url = 'http://localhost:8080/fcm'
WHERE fcm_service_account <> 'devbox-fake-fcm-service-account'
   OR fcm_url NOT LIKE 'http://localhost%';
