-- {"api":"PostRegistrationV2CreateFleet","migration":"capability","param":"fleet.onboarding.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'fleet.onboarding.write', 'DASHBOARD', 'PROVIDER_FLEET/REGISTRATION_V2/POST_REGISTRATION_V2_CREATE_FLEET' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostRegistrationV2AcceptFleetAssignment","migration":"capability","param":"fleet.onboarding.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'fleet.onboarding.write', 'DASHBOARD', 'PROVIDER_FLEET/REGISTRATION_V2/POST_REGISTRATION_V2_ACCEPT_FLEET_ASSIGNMENT' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetRegistrationV2Contact","migration":"capability","param":"fleet.onboarding.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'fleet.onboarding.read', 'DASHBOARD', 'PROVIDER_FLEET/REGISTRATION_V2/GET_REGISTRATION_V2_CONTACT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRegistrationV2Contact","migration":"capability","param":"fleet.onboarding.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'fleet.onboarding.write', 'DASHBOARD', 'PROVIDER_FLEET/REGISTRATION_V2/POST_REGISTRATION_V2_CONTACT' ) ON CONFLICT DO NOTHING;
