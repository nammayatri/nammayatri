-- {"api":"GetRideList","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT RIDES RIDE_LIST","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/RIDE/GET_RIDE_LIST' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'RIDE_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRideInfo","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS RIDE_INFO_CUSTOMER","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/RIDE/GET_RIDE_INFO' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'RIDE_INFO_CUSTOMER' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideSyncMultiple","migration":"endpoint","param":"RideAPI MultipleRideSyncRideEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC_MULTIPLE'
  WHERE endpoint = 'RideAPI MultipleRideSyncRideEndpoint';

-- {"api":"PostRideSyncMultiple","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT RIDES MULTIPLE_RIDE_SYNC","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC_MULTIPLE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'MULTIPLE_RIDE_SYNC' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideCancelMultiple","migration":"endpoint","param":"RideAPI MultipleRideCancelRideEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE'
  WHERE endpoint = 'RideAPI MultipleRideCancelRideEndpoint';

-- {"api":"PostRideCancelMultiple","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT RIDES MULTIPLE_RIDE_CANCEL","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'MULTIPLE_RIDE_CANCEL' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRideKaptureList","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT RIDES TICKET_RIDE_LIST_API","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/RIDE/GET_RIDE_KAPTURE_LIST' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'TICKET_RIDE_LIST_API' ) ON CONFLICT DO NOTHING;
