-- {"api":"TransitOperatorQueryVehicle","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_QUERY_VEHICLE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorUpsertVehicles","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_VEHICLES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorDeleteVehicle","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_DELETE_VEHICLE' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"TransitOperatorGetEtaVariants","migration":"capability","param":"transit-config.eta_variant.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-config.eta_variant.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_ETA_VARIANTS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorUpsertEtaVariant","migration":"capability","param":"transit-config.eta_variant.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-config.eta_variant.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_ETA_VARIANT' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorDeleteEtaVariant","migration":"capability","param":"transit-config.eta_variant.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-config.eta_variant.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_DELETE_ETA_VARIANT' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorUpsertStationEtas","migration":"capability","param":"transit-config.station_eta.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-config.station_eta.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_STATION_ETAS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorActiveTripEtaOverrides","migration":"capability","param":"transit-operations.eta_override.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.eta_override.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_ACTIVE_TRIP_ETA_OVERRIDES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorSetTripEtaOverride","migration":"capability","param":"transit-operations.eta_override.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.eta_override.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_SET_TRIP_ETA_OVERRIDE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorClearTripEtaOverride","migration":"capability","param":"transit-operations.eta_override.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.eta_override.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_CLEAR_TRIP_ETA_OVERRIDE' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"TransitOperatorGetStationEtas","migration":"capability","param":"transit-config.eta_variant.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-config.eta_variant.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_STATION_ETAS' ) ON CONFLICT DO NOTHING;
