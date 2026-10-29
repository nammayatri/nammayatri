-- {"api":"GetPersonList","migration":"capability","param":"admin.user.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'admin.user.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PERSON/GET_PERSON_LIST' ) ON CONFLICT DO NOTHING;
