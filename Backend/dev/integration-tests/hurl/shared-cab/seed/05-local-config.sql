-- Local-only DB config for the scenario run (never for a shared environment). After running it, clear the apps' Redis config
-- caches and restart the processes. vehicle_trip / frfs_ticket_booking go off KV: the drainer lag makes DB-range reads stale mid-scenario.
UPDATE atlas_app.system_configs SET config_value = replace(config_value, '"disableForKV": [', '"disableForKV": ["vehicle_trip","frfs_ticket_booking",')
 WHERE id = 'kv_configs' AND config_value NOT LIKE '%"vehicle_trip"%';
-- shared cab on for the test city (NULL would follow the SHARED_CAB IntegratedBPPConfig alone, false turns it off)
UPDATE atlas_app.rider_config SET shared_cab_enabled = true WHERE merchant_operating_city_id = 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874';
-- reconciler is gated per city
UPDATE atlas_driver_offer_bpp.transporter_config SET shared_cab_reconciler_enabled = true WHERE merchant_operating_city_id = 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da';
-- optional, for the finding-timeout scenario: 25 s instead of 20 min
-- UPDATE atlas_app.rider_config SET shared_cab_finding_timeout_sec = 25 WHERE merchant_operating_city_id = 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874';
