-- {"api":"GetCancellationConsequenceList","migration":"capability","param":"city-config.cancel.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.cancel.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/CANCELLATION_CONSEQUENCE/GET_CANCELLATION_CONSEQUENCE_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostCancellationConsequenceCreate","migration":"capability","param":"city-config.cancel.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.cancel.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/CANCELLATION_CONSEQUENCE/POST_CANCELLATION_CONSEQUENCE_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostCancellationConsequenceUpdate","migration":"capability","param":"city-config.cancel.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.cancel.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/CANCELLATION_CONSEQUENCE/POST_CANCELLATION_CONSEQUENCE_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetCancellationConsequenceRegistryList","migration":"capability","param":"city-config.cancel.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.cancel.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/CANCELLATION_CONSEQUENCE/GET_CANCELLATION_CONSEQUENCE_REGISTRY_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostCancellationConsequenceRegistryUpsert","migration":"capability","param":"city-config.cancel.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.cancel.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/CANCELLATION_CONSEQUENCE/POST_CANCELLATION_CONSEQUENCE_REGISTRY_UPSERT' ) ON CONFLICT DO NOTHING;
