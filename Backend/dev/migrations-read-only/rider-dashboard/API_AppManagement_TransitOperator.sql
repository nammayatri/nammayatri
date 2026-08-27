-- {"api":"TransitOperatorQueryVehicle","migration":"capability","param":"transit-operations.master.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_QUERY_VEHICLE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorUpsertVehicles","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_VEHICLES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorDeleteVehicle","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_DELETE_VEHICLE' ) ON CONFLICT DO NOTHING;
