-- {"api":"TransitOperatorV2ListLiveVehicles","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_LIVE/TRANSIT_OPERATOR_V2_LIST_LIVE_VEHICLES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GetLiveVehicle","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_LIVE/TRANSIT_OPERATOR_V2_GET_LIVE_VEHICLE' ) ON CONFLICT DO NOTHING;
