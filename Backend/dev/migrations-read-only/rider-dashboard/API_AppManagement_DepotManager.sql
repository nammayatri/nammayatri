-- {"api":"PostDepotManagerUpsertOne","migration":"capability","param":"city-operations.depot-manager.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.depot-manager.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/DEPOT_MANAGER/POST_DEPOT_MANAGER_UPSERT_ONE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDepotManagerUpsertMany","migration":"capability","param":"city-operations.depot-manager.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.depot-manager.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/DEPOT_MANAGER/POST_DEPOT_MANAGER_UPSERT_MANY' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDepotManagerList","migration":"capability","param":"city-operations.depot-manager.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.depot-manager.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/DEPOT_MANAGER/GET_DEPOT_MANAGER_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"DeleteDepotManager","migration":"capability","param":"city-operations.depot-manager.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.depot-manager.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/DEPOT_MANAGER/DELETE_DEPOT_MANAGER' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDepotManagerByMobileNumber","migration":"capability","param":"city-operations.depot-manager.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.depot-manager.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/DEPOT_MANAGER/GET_DEPOT_MANAGER_BY_MOBILE_NUMBER' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDepotManagerByDepotCode","migration":"capability","param":"city-operations.depot-manager.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.depot-manager.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/DEPOT_MANAGER/GET_DEPOT_MANAGER_BY_DEPOT_CODE' ) ON CONFLICT DO NOTHING;
