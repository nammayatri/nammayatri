-- {"api":"GetAppAttestationConfig","migration":"capability","param":"system-config.merchant.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.merchant.read', 'DASHBOARD', 'RIDER_MANAGEMENT/APP_ATTESTATION/GET_APP_ATTESTATION_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"PostAppAttestationConfig","migration":"capability","param":"system-config.merchant.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.merchant.write', 'DASHBOARD', 'RIDER_MANAGEMENT/APP_ATTESTATION/POST_APP_ATTESTATION_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"PostAppAttestationIosKeyRevoke","migration":"capability","param":"system-config.merchant.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.merchant.write', 'DASHBOARD', 'RIDER_MANAGEMENT/APP_ATTESTATION/POST_APP_ATTESTATION_IOS_KEY_REVOKE' ) ON CONFLICT DO NOTHING;
