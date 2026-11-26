-- {"api":"GetDriverAppAttestationConfig","migration":"capability","param":"system-config.merchant.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.merchant.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_APP_ATTESTATION/GET_DRIVER_APP_ATTESTATION_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverAppAttestationConfig","migration":"capability","param":"system-config.merchant.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_APP_ATTESTATION/POST_DRIVER_APP_ATTESTATION_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverAppAttestationIosKeyRevoke","migration":"capability","param":"system-config.merchant.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_APP_ATTESTATION/POST_DRIVER_APP_ATTESTATION_IOS_KEY_REVOKE' ) ON CONFLICT DO NOTHING;
