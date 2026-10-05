-- {"api":"GetPricingAdjustmentList","migration":"capability","param":"system-config.dynamic_logic.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/GET_PRICING_ADJUSTMENT_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPricingAdjustmentCreate","migration":"capability","param":"system-config.dynamic_logic.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/POST_PRICING_ADJUSTMENT_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPricingAdjustmentUpdate","migration":"capability","param":"system-config.dynamic_logic.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/POST_PRICING_ADJUSTMENT_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPricingAdjustmentStatus","migration":"capability","param":"system-config.dynamic_logic.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/POST_PRICING_ADJUSTMENT_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPricingAdjustmentPreview","migration":"capability","param":"system-config.dynamic_logic.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/POST_PRICING_ADJUSTMENT_PREVIEW' ) ON CONFLICT DO NOTHING;

-- {"api":"GetPricingAdjustmentResults","migration":"capability","param":"system-config.dynamic_logic.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/GET_PRICING_ADJUSTMENT_RESULTS' ) ON CONFLICT DO NOTHING;
