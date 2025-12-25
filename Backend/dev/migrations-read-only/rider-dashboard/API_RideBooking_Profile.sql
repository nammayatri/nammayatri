-- {"api":"GetProfileDetail","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS PERSONDETAIL","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/PROFILE/GET_PROFILE_DETAIL' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'PERSONDETAIL' ) ON CONFLICT DO NOTHING;

-- {"api":"PostProfileUpdate","migration":"endpoint","param":"ProfileAPI UpdatePersonEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/PROFILE/POST_PROFILE_UPDATE'
  WHERE endpoint = 'ProfileAPI UpdatePersonEndPoint';

-- {"api":"PostProfileUpdate","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS UPDATEPERSON","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/PROFILE/POST_PROFILE_UPDATE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'UPDATEPERSON' ) ON CONFLICT DO NOTHING;
