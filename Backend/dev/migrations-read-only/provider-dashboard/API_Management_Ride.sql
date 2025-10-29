-- {"api":"PostRideEndMultiple","migration":"endpoint","param":"RideAPI MultipleRideEndEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_END_MULTIPLE'
  WHERE endpoint = 'RideAPI MultipleRideEndEndpoint';

-- {"api":"PostRideEndMultiple","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_END_MULTIPLE'
  WHERE endpoint = 'RideAPI PostRideEndMultipleEndpoint';

-- {"api":"PostRideCancelMultiple","migration":"endpoint","param":"RideAPI MultipleRideCancelEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE'
  WHERE endpoint = 'RideAPI MultipleRideCancelEndpoint';

-- {"api":"PostRideCancelMultiple","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE'
  WHERE endpoint = 'RideAPI PostRideCancelMultipleEndpoint';

-- {"api":"PostRideSync","migration":"endpoint","param":"RideAPI RideSyncEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC'
  WHERE endpoint = 'RideAPI RideSyncEndpoint';

-- {"api":"PostRideSync","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC'
  WHERE endpoint = 'RideAPI PostRideSyncEndpoint';

-- {"api":"PostRideSyncMultiple","migration":"endpoint","param":"RideAPI MultipleRideSyncEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC_MULTIPLE'
  WHERE endpoint = 'RideAPI MultipleRideSyncEndpoint';

-- {"api":"PostRideSyncMultiple","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC_MULTIPLE'
  WHERE endpoint = 'RideAPI PostRideSyncMultipleEndpoint';

-- {"api":"GetRideKaptureList","migration":"endpoint","param":"RideAPI TicketRideListEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_KAPTURE_LIST'
  WHERE endpoint = 'RideAPI TicketRideListEndpoint';

-- {"api":"GetRideKaptureList","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_KAPTURE_LIST'
  WHERE endpoint = 'RideAPI GetRideKaptureListEndpoint';


------- SQL updates -------

-- {"api":"GetRideList","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT RIDES RIDE_LIST","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_LIST' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'RIDE_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideEndMultiple","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT RIDES MULTIPLE_RIDE_END","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_END_MULTIPLE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'MULTIPLE_RIDE_END' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideCancelMultiple","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT RIDES MULTIPLE_RIDE_CANCEL","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'MULTIPLE_RIDE_CANCEL' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRideInfo","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT RIDES RIDE_INFO","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_INFO' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'RIDE_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideSync","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT RIDES RIDE_SYNC","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'RIDE_SYNC' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideSyncMultiple","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT RIDES MULTIPLE_RIDE_SYNC","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC_MULTIPLE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'MULTIPLE_RIDE_SYNC' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideRoute","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT RIDES RIDE_ROUTE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_ROUTE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'RIDE_ROUTE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRideKaptureList","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT RIDES TICKET_RIDE_LIST_API","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_KAPTURE_LIST' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'TICKET_RIDE_LIST_API' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetRideFareBreakUp","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT RIDES FARE_BREAKUP","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_FARE_BREAK_UP' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'FARE_BREAKUP' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetRideListV2","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT RIDES RIDE_LIST_V2","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_LIST_V2' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'RIDE_LIST_V2' ) ON CONFLICT DO NOTHING;
