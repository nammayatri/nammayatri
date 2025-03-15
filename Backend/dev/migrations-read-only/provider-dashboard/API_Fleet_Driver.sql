-- {"api":"PostDriverFleetAddVehicles","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET ADD_VEHICLES","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_VEHICLES' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'ADD_VEHICLES' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverFleetAddVehicle","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET ADD_VEHICLE_FLEET","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_VEHICLE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'ADD_VEHICLE_FLEET' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverFleetGetDriverRequests","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET GET_DRIVER_REQUESTS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_GET_DRIVER_REQUESTS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'GET_DRIVER_REQUESTS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverFleetRespondDriverRequest","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET RESPOND_DRIVER_REQUEST","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_RESPOND_DRIVER_REQUEST' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'RESPOND_DRIVER_REQUEST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverFleetAddRCWithoutDriver","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET ADD_RC_FLEET_WITHOUT_DRIVER","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_RC_WITHOUT_DRIVER' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'ADD_RC_FLEET_WITHOUT_DRIVER' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverFleetGetAllVehicle","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET GET_ALL_VEHICLE_FOR_FLEET","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_GET_ALL_VEHICLE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'GET_ALL_VEHICLE_FOR_FLEET' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverFleetGetAllDriver","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET GET_ALL_DRIVERS_FOR_FLEET","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_GET_ALL_DRIVER' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'GET_ALL_DRIVERS_FOR_FLEET' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverFleetUnlink","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET FLEET_UNLINK_VEHICLE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_UNLINK' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'FLEET_UNLINK_VEHICLE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverFleetRemoveVehicle","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET FLEET_REMOVE_VEHICLE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_REMOVE_VEHICLE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'FLEET_REMOVE_VEHICLE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverFleetRemoveDriver","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET FLEET_REMOVE_DRIVER","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_REMOVE_DRIVER' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'FLEET_REMOVE_DRIVER' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverFleetTotalEarning","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET FLEET_TOTAL_EARNING","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_TOTAL_EARNING' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'FLEET_TOTAL_EARNING' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverFleetVehicleEarning","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET FLEET_VEHICLE_EARNING","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_VEHICLE_EARNING' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'FLEET_VEHICLE_EARNING' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverFleetDriverEarning","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET FLEET_DRIVER_EARNING","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DRIVER_EARNING' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'FLEET_DRIVER_EARNING' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverFleetDriverVehicleAssociation","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET GET_DRIVER_VEHICLE_ASSOCIATION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DRIVER_VEHICLE_ASSOCIATION' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'GET_DRIVER_VEHICLE_ASSOCIATION' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverFleetDriverAssociation","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET GET_DRIVER_ASSOCIATION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DRIVER_ASSOCIATION' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'GET_DRIVER_ASSOCIATION' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverFleetVehicleAssociation","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET GET_VEHICLE_ASSOCIATION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_VEHICLE_ASSOCIATION' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'GET_VEHICLE_ASSOCIATION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverFleetVehicleDriverRcStatus","migration":"endpoint","param":"DriverAPI SetVehicleDriverRcStatusForFleetEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VEHICLE_DRIVER_RC_STATUS'
  WHERE endpoint = 'DriverAPI SetVehicleDriverRcStatusForFleetEndpoint';

-- {"api":"PostDriverFleetVehicleDriverRcStatus","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VEHICLE_DRIVER_RC_STATUS'
  WHERE endpoint = 'DriverAPI PostDriverFleetVehicleDriverRcStatusEndpoint';

-- {"api":"PostDriverFleetVehicleDriverRcStatus","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET SET_VEHICLE_DRIVER_RC_STATUS_FOR_FLEET","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VEHICLE_DRIVER_RC_STATUS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'SET_VEHICLE_DRIVER_RC_STATUS_FOR_FLEET' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverUpdateFleetOwnerInfo","migration":"endpoint","param":"DriverAPI UpdateFleetOwnerEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_FLEET/DRIVER/POST_DRIVER_UPDATE_FLEET_OWNER_INFO'
  WHERE endpoint = 'DriverAPI UpdateFleetOwnerEndPoint';

-- {"api":"PostDriverUpdateFleetOwnerInfo","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_FLEET/DRIVER/POST_DRIVER_UPDATE_FLEET_OWNER_INFO'
  WHERE endpoint = 'DriverAPI PostDriverUpdateFleetOwnerInfoEndpoint';

-- {"api":"PostDriverUpdateFleetOwnerInfo","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET UPDATE_FLEET_OWNER_INFO","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_UPDATE_FLEET_OWNER_INFO' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'UPDATE_FLEET_OWNER_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverFleetOwnerInfo","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET GET_FLEET_OWNER_INFO","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_OWNER_INFO' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'GET_FLEET_OWNER_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverFleetSendJoiningOtp","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET SEND_FLEET_JOINING_OTP","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_SEND_JOINING_OTP' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'SEND_FLEET_JOINING_OTP' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverFleetVerifyJoiningOtp","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET VERIFY_FLEET_JOINING_OTP","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VERIFY_JOINING_OTP' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'VERIFY_FLEET_JOINING_OTP' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverFleetRoutes","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET WMB_ROUTES_API","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_ROUTES' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'WMB_ROUTES_API' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverFleetPossibleRoutes","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET WMB_POSSIBLE_ROUTES_API","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_POSSIBLE_ROUTES' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'WMB_POSSIBLE_ROUTES_API' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverFleetTripPlanner","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET WMB_TRIP_PLANNER_API","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_TRIP_PLANNER' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'WMB_TRIP_PLANNER_API' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverFleetTripTransactions","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET WMB_TRIP_TRANSACTIONS_API","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_TRIP_TRANSACTIONS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'WMB_TRIP_TRANSACTIONS_API' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverFleetAddDrivers","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET ADD_DRIVERS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_DRIVERS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'ADD_DRIVERS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverFleetAddDriverBusRouteMapping","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET ADD_DRIVER_BUS_ROUTE_MAPPING","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_DRIVER_BUS_ROUTE_MAPPING' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'ADD_DRIVER_BUS_ROUTE_MAPPING' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverFleetLinkRCWithDriver","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT FLEET LINK_RC_WITH_DRIVER","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_LINK_RC_WITH_DRIVER' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'FLEET' AND T1.user_action_type = 'LINK_RC_WITH_DRIVER' ) ON CONFLICT DO NOTHING;
