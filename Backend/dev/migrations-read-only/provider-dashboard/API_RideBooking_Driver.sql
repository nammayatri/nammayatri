-- {"api":"PostDriverEnable","migration":"endpoint","param":"DriverAPI EnableDriverEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_ENABLE'
  WHERE endpoint = 'DriverAPI EnableDriverEndpoint';

-- {"api":"PostDriverEnable","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_ENABLE'
  WHERE endpoint = 'DriverAPI PostDriverEnableEndpoint';

-- {"api":"PostDriverCollectCash","migration":"endpoint","param":"DriverAPI CollectCashEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_COLLECT_CASH'
  WHERE endpoint = 'DriverAPI CollectCashEndpoint';

-- {"api":"PostDriverCollectCash","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_COLLECT_CASH'
  WHERE endpoint = 'DriverAPI PostDriverCollectCashEndpoint';

-- {"api":"PostDriverV2CollectCash","migration":"endpoint","param":"DriverAPI CollectCashEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_V2_COLLECT_CASH'
  WHERE endpoint = 'DriverAPI CollectCashEndpoint';

-- {"api":"PostDriverV2CollectCash","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_V2_COLLECT_CASH'
  WHERE endpoint = 'DriverAPI PostDriverV2CollectCashEndpoint';

-- {"api":"PostDriverExemptCash","migration":"endpoint","param":"DriverAPI ExemptCashEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_EXEMPT_CASH'
  WHERE endpoint = 'DriverAPI ExemptCashEndpoint';

-- {"api":"PostDriverExemptCash","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_EXEMPT_CASH'
  WHERE endpoint = 'DriverAPI PostDriverExemptCashEndpoint';

-- {"api":"PostDriverV2ExemptCash","migration":"endpoint","param":"DriverAPI ExemptCashEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_V2_EXEMPT_CASH'
  WHERE endpoint = 'DriverAPI ExemptCashEndpoint';

-- {"api":"PostDriverV2ExemptCash","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_V2_EXEMPT_CASH'
  WHERE endpoint = 'DriverAPI PostDriverV2ExemptCashEndpoint';

-- {"api":"PostDriverUnlinkVehicle","migration":"endpoint","param":"DriverAPI UnlinkVehicleEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_UNLINK_VEHICLE'
  WHERE endpoint = 'DriverAPI UnlinkVehicleEndpoint';

-- {"api":"PostDriverUnlinkVehicle","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_UNLINK_VEHICLE'
  WHERE endpoint = 'DriverAPI PostDriverUnlinkVehicleEndpoint';

-- {"api":"PostDriverEndRCAssociation","migration":"endpoint","param":"DriverAPI EndRCAssociationEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_END_RC_ASSOCIATION'
  WHERE endpoint = 'DriverAPI EndRCAssociationEndpoint';

-- {"api":"PostDriverEndRCAssociation","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_END_RC_ASSOCIATION'
  WHERE endpoint = 'DriverAPI PostDriverEndRCAssociationEndpoint';

-- {"api":"PostDriverAddVehicle","migration":"endpoint","param":"DriverAPI AddVehicleEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_ADD_VEHICLE'
  WHERE endpoint = 'DriverAPI AddVehicleEndpoint';

-- {"api":"PostDriverAddVehicle","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_ADD_VEHICLE'
  WHERE endpoint = 'DriverAPI PostDriverAddVehicleEndpoint';

-- {"api":"PostDriverSetRCStatus","migration":"endpoint","param":"DriverAPI SetRCStatusEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_SET_RC_STATUS'
  WHERE endpoint = 'DriverAPI SetRCStatusEndpoint';

-- {"api":"PostDriverSetRCStatus","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_SET_RC_STATUS'
  WHERE endpoint = 'DriverAPI PostDriverSetRCStatusEndpoint';


------- SQL updates -------

-- {"api":"PostDriverExemptDriverFee","migration":"endpoint","param":"DriverAPI ExemptionAndCashCollectionDriverFeeEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_EXEMPT_DRIVER_FEE'
  WHERE endpoint = 'DriverAPI ExemptionAndCashCollectionDriverFeeEndpoint';


------- SQL updates -------

-- {"api":"GetDriverPaymentDue","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP DRIVERS BALANCE_DUE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/DRIVER/GET_DRIVER_PAYMENT_DUE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'BALANCE_DUE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverEnable","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP DRIVERS ENABLE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_ENABLE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'ENABLE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverCollectCash","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP DRIVERS COLLECT_CASH","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_COLLECT_CASH' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'COLLECT_CASH' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverV2CollectCash","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP DRIVERS COLLECT_CASH_V2","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_V2_COLLECT_CASH' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'COLLECT_CASH_V2' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverExemptCash","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP DRIVERS EXEMPT_CASH","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_EXEMPT_CASH' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'EXEMPT_CASH' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverV2ExemptCash","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP DRIVERS EXEMPT_CASH_V2","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_V2_EXEMPT_CASH' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'EXEMPT_CASH_V2' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverInfo","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP DRIVERS INFO","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/DRIVER/GET_DRIVER_INFO' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverUnlinkVehicle","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP DRIVERS UNLINK_VEHICLE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_UNLINK_VEHICLE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'UNLINK_VEHICLE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverEndRCAssociation","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP DRIVERS END_RC_ASSOCIATION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_END_RC_ASSOCIATION' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'END_RC_ASSOCIATION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverAddVehicle","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP DRIVERS ADD_VEHICLE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_ADD_VEHICLE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'ADD_VEHICLE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverSetRCStatus","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP DRIVERS SET_RC_STATUS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_SET_RC_STATUS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'SET_RC_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverExemptDriverFee","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP DRIVERS EXEMPT_DRIVER_FEE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_EXEMPT_DRIVER_FEE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'EXEMPT_DRIVER_FEE' ) ON CONFLICT DO NOTHING;
