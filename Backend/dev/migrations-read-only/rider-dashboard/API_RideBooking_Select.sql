-- {"api":"PostSelectEstimate","migration":"endpoint","param":"SelectAPI EstimatesEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/SELECT/POST_SELECT_ESTIMATE'
  WHERE endpoint = 'SelectAPI EstimatesEndPoint';

-- {"api":"PostSelectEstimate","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS SELECT","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/SELECT/POST_SELECT_ESTIMATE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'SELECT' ) ON CONFLICT DO NOTHING;

-- {"api":"GetSelectQuotes","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS SELECTLIST","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/SELECT/GET_SELECT_QUOTES' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'SELECTLIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetSelectResult","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS SELECTRESULT","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/SELECT/GET_SELECT_RESULT' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'SELECTRESULT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostSelectCancelSearch","migration":"endpoint","param":"SelectAPI CancelSearchEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/SELECT/POST_SELECT_CANCEL_SEARCH'
  WHERE endpoint = 'SelectAPI CancelSearchEndPoint';

-- {"api":"PostSelectCancelSearch","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS CANCELSEARCH","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/SELECT/POST_SELECT_CANCEL_SEARCH' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'CANCELSEARCH' ) ON CONFLICT DO NOTHING;
