-- {"api":"ListPassCatalog","migration":"capability","param":"city-operations.pass.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/LIST_PASS_CATALOG' ) ON CONFLICT DO NOTHING;

-- {"api":"CreatePass","migration":"capability","param":"city-operations.pass.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/CREATE_PASS' ) ON CONFLICT DO NOTHING;

-- {"api":"UpdatePass","migration":"capability","param":"city-operations.pass.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS' ) ON CONFLICT DO NOTHING;

-- {"api":"DeletePass","migration":"capability","param":"city-operations.pass.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/DELETE_PASS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetPassCatalog","migration":"capability","param":"city-operations.pass.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/GET_PASS_CATALOG' ) ON CONFLICT DO NOTHING;

-- {"api":"ListPassType","migration":"capability","param":"city-operations.pass.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/LIST_PASS_TYPE' ) ON CONFLICT DO NOTHING;

-- {"api":"CreatePassType","migration":"capability","param":"city-operations.pass.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/CREATE_PASS_TYPE' ) ON CONFLICT DO NOTHING;

-- {"api":"UpdatePassType","migration":"capability","param":"city-operations.pass.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS_TYPE' ) ON CONFLICT DO NOTHING;

-- {"api":"ListPassCategory","migration":"capability","param":"city-operations.pass.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/LIST_PASS_CATEGORY' ) ON CONFLICT DO NOTHING;

-- {"api":"CreatePassCategory","migration":"capability","param":"city-operations.pass.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/CREATE_PASS_CATEGORY' ) ON CONFLICT DO NOTHING;

-- {"api":"UpdatePassCategory","migration":"capability","param":"city-operations.pass.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/UPDATE_PASS_CATEGORY' ) ON CONFLICT DO NOTHING;

-- {"api":"GetPurchasedPassPaymentTrips","migration":"capability","param":"city-operations.pass.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/GET_PURCHASED_PASS_PAYMENT_TRIPS' ) ON CONFLICT DO NOTHING;

-- {"api":"AdjustPurchasedPassPaymentTrips","migration":"capability","param":"city-operations.pass.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.pass.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/ADJUST_PURCHASED_PASS_PAYMENT_TRIPS' ) ON CONFLICT DO NOTHING;
