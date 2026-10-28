-- {"api":"GetMinimumGuaranteeInvitations","migration":"capability","param":"fleet.driver.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'fleet.driver.read', 'DASHBOARD', 'PROVIDER_FLEET/MINIMUM_GUARANTEE/GET_MINIMUM_GUARANTEE_INVITATIONS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMinimumGuaranteeOptIn","migration":"capability","param":"fleet.driver.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_OPT_IN' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMinimumGuaranteeOptOut","migration":"capability","param":"fleet.driver.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_OPT_OUT' ) ON CONFLICT DO NOTHING;
