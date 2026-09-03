-- {"api":"ListPassCatalog","migration":"capability","param":"city-operations.pass.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/LIST_PASS_CATALOG' ) ON CONFLICT DO NOTHING;

-- {"api":"CreatePass","migration":"capability","param":"city-operations.pass.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/CREATE_PASS' ) ON CONFLICT DO NOTHING;

-- {"api":"UpdatePass","migration":"capability","param":"city-operations.pass.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS' ) ON CONFLICT DO NOTHING;

-- {"api":"DeletePass","migration":"capability","param":"city-operations.pass.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/DELETE_PASS' ) ON CONFLICT DO NOTHING;
