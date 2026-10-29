-- {"api":"GetTranslationDomains","migration":"capability","param":"system-config.translation.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.translation.read', 'DASHBOARD', 'RIDER_MANAGEMENT/TRANSLATION/GET_TRANSLATION_DOMAINS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetTranslationKeys","migration":"capability","param":"system-config.translation.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.translation.read', 'DASHBOARD', 'RIDER_MANAGEMENT/TRANSLATION/GET_TRANSLATION_KEYS' ) ON CONFLICT DO NOTHING;
