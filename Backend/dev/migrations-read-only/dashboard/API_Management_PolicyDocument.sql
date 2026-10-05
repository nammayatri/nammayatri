-- {"api":"PostPolicyDocumentCreate","migration":"capability","param":"system-config.legal_compliance.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.legal_compliance.write', 'DASHBOARD', 'RIDER_MANAGEMENT/POLICY_DOCUMENT/POST_POLICY_DOCUMENT_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPolicyDocumentUpdate","migration":"capability","param":"system-config.legal_compliance.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.legal_compliance.write', 'DASHBOARD', 'RIDER_MANAGEMENT/POLICY_DOCUMENT/POST_POLICY_DOCUMENT_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetPolicyDocumentList","migration":"capability","param":"system-config.legal_compliance.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.legal_compliance.read', 'DASHBOARD', 'RIDER_MANAGEMENT/POLICY_DOCUMENT/GET_POLICY_DOCUMENT_LIST' ) ON CONFLICT DO NOTHING;
