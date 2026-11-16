-- {"api":"TransitV1ListSchedules","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_LIST_SCHEDULES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1CreateSchedule","migration":"capability","param":"transit-operations.waybill.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_CREATE_SCHEDULE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1GetSchedule","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_GET_SCHEDULE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1UpdateSchedule","migration":"capability","param":"transit-operations.waybill.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_UPDATE_SCHEDULE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1DeleteSchedule","migration":"capability","param":"transit-operations.waybill.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_DELETE_SCHEDULE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1ListScheduleTypes","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_LIST_SCHEDULE_TYPES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1ListScheduleTrips","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_LIST_SCHEDULE_TRIPS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1CreateScheduleTrip","migration":"capability","param":"transit-operations.waybill.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_CREATE_SCHEDULE_TRIP' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1GetScheduleTrip","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_GET_SCHEDULE_TRIP' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1UpdateScheduleTrip","migration":"capability","param":"transit-operations.waybill.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_UPDATE_SCHEDULE_TRIP' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1DeleteScheduleTrip","migration":"capability","param":"transit-operations.waybill.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_DELETE_SCHEDULE_TRIP' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1GetRepeatConfig","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_GET_REPEAT_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1SetRepeatConfig","migration":"capability","param":"transit-operations.waybill.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_SET_REPEAT_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1ListTripDetails","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_LIST_TRIP_DETAILS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1ReplaceTripDetails","migration":"capability","param":"transit-operations.waybill.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_REPLACE_TRIP_DETAILS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1UpdateTripDetail","migration":"capability","param":"transit-operations.waybill.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_UPDATE_TRIP_DETAIL' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1DeleteTripDetail","migration":"capability","param":"transit-operations.waybill.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_DELETE_TRIP_DETAIL' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1ListWaybills","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_LIST_WAYBILLS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1CreateWaybill","migration":"capability","param":"transit-operations.waybill.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_CREATE_WAYBILL' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1GetWaybill","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_GET_WAYBILL' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1UpdateWaybill","migration":"capability","param":"transit-operations.waybill.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_UPDATE_WAYBILL' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1DeleteWaybill","migration":"capability","param":"transit-operations.waybill.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_DELETE_WAYBILL' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1PreviewWaybillRepeats","migration":"capability","param":"transit-operations.trip.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.trip.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_PREVIEW_WAYBILL_REPEATS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitV1GenerateWaybillRepeats","migration":"capability","param":"transit-operations.trip.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.trip.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_V1/TRANSIT_V1_GENERATE_WAYBILL_REPEATS' ) ON CONFLICT DO NOTHING;
