-- {"api":"TransitOperatorQueryVehicle","migration":"capability","param":"transit-operations.master.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_QUERY_VEHICLE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorUpsertVehicles","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_VEHICLES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorDeleteVehicle","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_DELETE_VEHICLE' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"TransitOperatorV2ListTripGroups","migration":"capability","param":"transit-operations.master.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_LIST_TRIP_GROUPS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2UpsertTripGroup","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPSERT_TRIP_GROUP' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GetTripGroup","migration":"capability","param":"transit-operations.master.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_GET_TRIP_GROUP' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2DeleteTripGroup","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_DELETE_TRIP_GROUP' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListTrips","migration":"capability","param":"transit-operations.master.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_LIST_TRIPS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2UpsertTrips","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPSERT_TRIPS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2DeleteTrip","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_DELETE_TRIP' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListDutyRepeats","migration":"capability","param":"transit-operations.master.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_LIST_DUTY_REPEATS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2UpsertDutyRepeat","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPSERT_DUTY_REPEAT' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2DeleteDutyRepeat","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_DELETE_DUTY_REPEAT' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2PreviewDutyRepeats","migration":"capability","param":"transit-operations.master.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_PREVIEW_DUTY_REPEATS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GenerateDutyRepeats","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_GENERATE_DUTY_REPEATS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListDutyGroups","migration":"capability","param":"transit-operations.master.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_LIST_DUTY_GROUPS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2CreateDutyGroup","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_CREATE_DUTY_GROUP' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GetDutyGroup","migration":"capability","param":"transit-operations.master.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_GET_DUTY_GROUP' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListDuties","migration":"capability","param":"transit-operations.master.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_LIST_DUTIES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GetDutyGroupLogs","migration":"capability","param":"transit-operations.master.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_GET_DUTY_GROUP_LOGS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2UpdateDutyGroupVehicle","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_VEHICLE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2UpdateDutyGroupCrew","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_CREW' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2SetDutyGroupActive","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_SET_DUTY_GROUP_ACTIVE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2DeleteDutyGroup","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_DELETE_DUTY_GROUP' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2UpdateDutyCrew","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPDATE_DUTY_CREW' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2DeleteDuty","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_DELETE_DUTY' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListGenerationFailures","migration":"capability","param":"transit-operations.master.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_LIST_GENERATION_FAILURES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ResolveGenerationFailure","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_RESOLVE_GENERATION_FAILURE' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"TransitOperatorGetScheduleTripRepeat","migration":"capability","param":"transit-operations.master.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_SCHEDULE_TRIP_REPEAT' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorSetScheduleTripRepeat","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_SET_SCHEDULE_TRIP_REPEAT' ) ON CONFLICT DO NOTHING;
