-- {"api":"TransitOperatorV2ListOrganizations","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_ORGANIZATIONS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GetOrganization","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_GET_ORGANIZATION' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2CreateOrganization","migration":"capability","param":"transit-operations.org.admin","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.org.admin', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_CREATE_ORGANIZATION' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2UpdateOrganization","migration":"capability","param":"transit-operations.org.admin","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.org.admin', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_UPDATE_ORGANIZATION' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2DeleteOrganization","migration":"capability","param":"transit-operations.org.admin","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.org.admin', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_ORGANIZATION' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListDepots","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_DEPOTS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GetDepot","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_GET_DEPOT' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2CreateDepot","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_CREATE_DEPOT' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2UpdateDepot","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_UPDATE_DEPOT' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2DeleteDepot","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_DEPOT' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListVehicles","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_VEHICLES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GetVehicle","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_GET_VEHICLE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2CreateVehicle","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_CREATE_VEHICLE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2UpdateVehicle","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_UPDATE_VEHICLE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2DeleteVehicle","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_VEHICLE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2BulkVehicles","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_BULK_VEHICLES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListDevices","migration":"capability","param":"transit-operations.device.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.device.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_DEVICES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2BulkDevices","migration":"capability","param":"transit-operations.device.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.device.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_BULK_DEVICES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2DeleteDevice","migration":"capability","param":"transit-operations.device.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.device.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_DEVICE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListEmployees","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_EMPLOYEES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GetEmployee","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_GET_EMPLOYEE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2CreateEmployee","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_CREATE_EMPLOYEE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2UpdateEmployee","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_UPDATE_EMPLOYEE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2DeleteEmployee","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_EMPLOYEE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2BulkEmployees","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_BULK_EMPLOYEES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2EmployeeLogin","migration":"capability","param":"transit-operations.trip.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.trip.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_EMPLOYEE_LOGIN' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2EmployeeVerify","migration":"capability","param":"transit-operations.trip.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.trip.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_EMPLOYEE_VERIFY' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListDesignations","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_DESIGNATIONS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListCrew","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_CREW' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GetLookups","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_GET_LOOKUPS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListServiceTypes","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_SERVICE_TYPES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2GetServiceType","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_GET_SERVICE_TYPE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2CreateServiceType","migration":"capability","param":"transit-operations.org.admin","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.org.admin', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_CREATE_SERVICE_TYPE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2UpdateServiceType","migration":"capability","param":"transit-operations.org.admin","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.org.admin', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_UPDATE_SERVICE_TYPE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2DeleteServiceType","migration":"capability","param":"transit-operations.org.admin","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.org.admin', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_SERVICE_TYPE' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListStationEtas","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_STATION_ETAS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2PutStationEtas","migration":"capability","param":"transit-operations.org.admin","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.org.admin', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_PUT_STATION_ETAS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListGtfsRoutes","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_GTFS_ROUTES' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2ListGtfsRouteStops","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_GTFS_ROUTE_STOPS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2SearchGtfsStops","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_SEARCH_GTFS_STOPS' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorV2AssignPersonOrganization","migration":"capability","param":"transit-operations.org.admin","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.org.admin', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_ASSIGN_PERSON_ORGANIZATION' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"TransitOperatorV2ListPersons","migration":"capability","param":"transit-operations.org.admin","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.org.admin', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_PERSONS' ) ON CONFLICT DO NOTHING;
