-- Runs after config-sync (files here are applied in name order by
-- test-tool/context-api/server.py _apply_local_testing_data), before
-- rider-app / driver-app are restarted.
--
-- The synced Maps_Disha service config points osrmUrl at a private cloud IP
-- (http://10.60.31.161:5000) that a local / devbox machine cannot reach, so
-- route calls fail with "No route to host" (e.g. rider pickup route after the
-- OTP screen). config-sync's patches.json only rewrites http://beckn-osrm, so
-- point any remaining unreachable OSRM URL at the local OSRM server instead.
UPDATE atlas_app.merchant_service_config
SET config_json = (config_json::jsonb || '{"osrmUrl": "http://localhost:5001"}')::json
WHERE config_json->>'osrmUrl' = 'http://10.60.31.161:5000';

UPDATE atlas_driver_offer_bpp.merchant_service_config
SET config_json = (config_json::jsonb || '{"osrmUrl": "http://localhost:5001"}')::json
WHERE config_json->>'osrmUrl' = 'http://10.60.31.161:5000';
