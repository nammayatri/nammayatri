-- {"api":"ListParkingClusters","migration":"capability","param":"city-operations.parking.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.parking.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PARKING/LIST_PARKING_CLUSTERS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetParkingCityRevenue","migration":"capability","param":"city-operations.parking.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.parking.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PARKING/GET_PARKING_CITY_REVENUE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetParkingCluster","migration":"capability","param":"city-operations.parking_cluster.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.parking_cluster.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PARKING/GET_PARKING_CLUSTER' ) ON CONFLICT DO NOTHING;

-- {"api":"SetParkingClusterStatus","migration":"capability","param":"city-operations.parking_cluster.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.parking_cluster.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PARKING/SET_PARKING_CLUSTER_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetParkingOccupancy","migration":"capability","param":"city-operations.parking_cluster.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.parking_cluster.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PARKING/GET_PARKING_OCCUPANCY' ) ON CONFLICT DO NOTHING;

-- {"api":"GetParkingClusterRevenue","migration":"capability","param":"city-operations.parking_revenue.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.parking_revenue.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PARKING/GET_PARKING_CLUSTER_REVENUE' ) ON CONFLICT DO NOTHING;

-- {"api":"VerifyParkingTicket","migration":"capability","param":"city-operations.parking_marshal.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.parking_marshal.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PARKING/VERIFY_PARKING_TICKET' ) ON CONFLICT DO NOTHING;

-- {"api":"CashBookParking","migration":"capability","param":"city-operations.parking_marshal.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.parking_marshal.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PARKING/CASH_BOOK_PARKING' ) ON CONFLICT DO NOTHING;
