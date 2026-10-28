-- {"api":"GetMinimumGuaranteeList","migration":"capability","param":"system-config.coins.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/GET_MINIMUM_GUARANTEE_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMinimumGuaranteeCreate","migration":"capability","param":"system-config.coins.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMinimumGuaranteeUpdate","migration":"capability","param":"system-config.coins.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMinimumGuaranteeInvite","migration":"capability","param":"system-config.coins.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_INVITE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMinimumGuaranteeStatus","migration":"capability","param":"system-config.coins.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMinimumGuaranteeExtend","migration":"capability","param":"system-config.coins.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_EXTEND' ) ON CONFLICT DO NOTHING;

-- {"api":"GetMinimumGuaranteeEnrollments","migration":"capability","param":"system-config.coins.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/GET_MINIMUM_GUARANTEE_ENROLLMENTS' ) ON CONFLICT DO NOTHING;
