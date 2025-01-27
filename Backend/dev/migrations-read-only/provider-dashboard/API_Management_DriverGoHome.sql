-- {"api":"PostDriverGoHomeUpdateHomeLocation","migration":"endpoint","param":"DriverAPI UpdateDriverHomeLocationEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/POST_DRIVER_GO_HOME_UPDATE_HOME_LOCATION'
  WHERE endpoint = 'DriverAPI UpdateDriverHomeLocationEndpoint';

-- {"api":"PostDriverGoHomeUpdateHomeLocation","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/POST_DRIVER_GO_HOME_UPDATE_HOME_LOCATION'
  WHERE endpoint = 'DriverGoHomeAPI PostDriverGoHomeUpdateHomeLocationEndpoint';

-- {"api":"PostDriverGoHomeIncrementGoToCount","migration":"endpoint","param":"DriverAPI IncrementDriverGoToCountEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/POST_DRIVER_GO_HOME_INCREMENT_GO_TO_COUNT'
  WHERE endpoint = 'DriverAPI IncrementDriverGoToCountEndPoint';

-- {"api":"PostDriverGoHomeIncrementGoToCount","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/POST_DRIVER_GO_HOME_INCREMENT_GO_TO_COUNT'
  WHERE endpoint = 'DriverGoHomeAPI PostDriverGoHomeIncrementGoToCountEndpoint';


------- SQL updates -------

-- {"api":"GetDriverGoHomeGetHomeLocation","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS GET_DRIVER_HOME_LOCATION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/GET_DRIVER_GO_HOME_GET_HOME_LOCATION' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'GET_DRIVER_HOME_LOCATION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverGoHomeUpdateHomeLocation","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS UPDATE_DRIVER_HOME_LOCATION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/POST_DRIVER_GO_HOME_UPDATE_HOME_LOCATION' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'UPDATE_DRIVER_HOME_LOCATION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverGoHomeIncrementGoToCount","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS INCREMENT_DRIVER_GO_TO_COUNT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/POST_DRIVER_GO_HOME_INCREMENT_GO_TO_COUNT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'INCREMENT_DRIVER_GO_TO_COUNT' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverGoHomeGetGoHomeInfo","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS GET_DRIVER_GO_HOME_INFO","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/GET_DRIVER_GO_HOME_GET_GO_HOME_INFO' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'GET_DRIVER_GO_HOME_INFO' ) ON CONFLICT DO NOTHING;
