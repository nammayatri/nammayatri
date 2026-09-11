-- {"api":"ListPassCatalog","migration":"capability","param":"city-config.pass_catalog.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.pass_catalog.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/LIST_PASS_CATALOG' ) ON CONFLICT DO NOTHING;

-- {"api":"CreatePass","migration":"capability","param":"city-config.pass_catalog.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.pass_catalog.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/CREATE_PASS' ) ON CONFLICT DO NOTHING;

-- {"api":"UpdatePass","migration":"capability","param":"city-config.pass_catalog.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.pass_catalog.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS' ) ON CONFLICT DO NOTHING;

-- {"api":"DeletePass","migration":"capability","param":"city-config.pass_catalog.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.pass_catalog.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/DELETE_PASS' ) ON CONFLICT DO NOTHING;

-- {"api":"ListPassCategories","migration":"capability","param":"city-config.pass_catalog.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.pass_catalog.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/LIST_PASS_CATEGORIES' ) ON CONFLICT DO NOTHING;

-- {"api":"CreatePassCategory","migration":"capability","param":"city-config.pass_catalog.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.pass_catalog.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/CREATE_PASS_CATEGORY' ) ON CONFLICT DO NOTHING;

-- {"api":"UpdatePassCategory","migration":"capability","param":"city-config.pass_catalog.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.pass_catalog.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS_CATEGORY' ) ON CONFLICT DO NOTHING;

-- {"api":"ListPassTypes","migration":"capability","param":"city-config.pass_catalog.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.pass_catalog.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/LIST_PASS_TYPES' ) ON CONFLICT DO NOTHING;

-- {"api":"CreatePassType","migration":"capability","param":"city-config.pass_catalog.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.pass_catalog.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/CREATE_PASS_TYPE' ) ON CONFLICT DO NOTHING;

-- {"api":"UpdatePassType","migration":"capability","param":"city-config.pass_catalog.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.pass_catalog.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS_TYPE' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetPassOverrideConfig","migration":"capability","param":"city-config.pass_catalog.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.pass_catalog.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/GET_PASS_OVERRIDE_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"UpdatePassOverrideConfig","migration":"capability","param":"city-config.pass_catalog.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.pass_catalog.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS_OVERRIDE_CONFIG' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostPassTripsAdjust","migration":"capability","param":"city-operations.pass.execute","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/POST_PASS_TRIPS_ADJUST' ) ON CONFLICT DO NOTHING;
