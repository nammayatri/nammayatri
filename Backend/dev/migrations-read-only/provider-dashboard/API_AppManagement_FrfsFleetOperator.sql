-- {"api":"PostFrfsFleetOperatorCurrentOperation","migration":"capability","param":"transit-operations.trip.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.trip.execute', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/FRFS_FLEET_OPERATOR/POST_FRFS_FLEET_OPERATOR_CURRENT_OPERATION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFrfsFleetOperatorTripAction","migration":"capability","param":"transit-operations.trip.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.trip.execute', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/FRFS_FLEET_OPERATOR/POST_FRFS_FLEET_OPERATOR_TRIP_ACTION' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostFrfsFleetOperatorV2CurrentOperation","migration":"capability","param":"transit-operations.trip.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.trip.execute', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/FRFS_FLEET_OPERATOR/POST_FRFS_FLEET_OPERATOR_V2_CURRENT_OPERATION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFrfsFleetOperatorV2TripAction","migration":"capability","param":"transit-operations.trip.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.trip.execute', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/FRFS_FLEET_OPERATOR/POST_FRFS_FLEET_OPERATOR_V2_TRIP_ACTION' ) ON CONFLICT DO NOTHING;
