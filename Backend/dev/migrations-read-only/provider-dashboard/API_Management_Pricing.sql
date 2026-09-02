-- {"api":"GetPricingSurgeList","migration":"capability","param":"system-config.dynamic_logic.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PRICING/GET_PRICING_SURGE_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPricingSurgeCreate","migration":"capability","param":"system-config.dynamic_logic.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PRICING/POST_PRICING_SURGE_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPricingSurgeUpdate","migration":"capability","param":"system-config.dynamic_logic.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PRICING/POST_PRICING_SURGE_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPricingSurgeStatus","migration":"capability","param":"system-config.dynamic_logic.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PRICING/POST_PRICING_SURGE_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPricingSurgePreview","migration":"capability","param":"system-config.dynamic_logic.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PRICING/POST_PRICING_SURGE_PREVIEW' ) ON CONFLICT DO NOTHING;

-- {"api":"GetPricingObservabilityEstimate","migration":"capability","param":"system-config.dynamic_logic.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PRICING/GET_PRICING_OBSERVABILITY_ESTIMATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetPricingObservabilityHealth","migration":"capability","param":"system-config.dynamic_logic.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PRICING/GET_PRICING_OBSERVABILITY_HEALTH' ) ON CONFLICT DO NOTHING;
