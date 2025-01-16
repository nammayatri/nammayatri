-- {"api":"GetFrontendFlowStatus","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS FLOW_STATUS","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/FRONTEND/GET_FRONTEND_FLOW_STATUS' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'FLOW_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFrontendNotifyEvent","migration":"endpoint","param":"FlowStatusAPI NotifyEventEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/FRONTEND/POST_FRONTEND_NOTIFY_EVENT'
  WHERE endpoint = 'FlowStatusAPI NotifyEventEndPoint';

-- {"api":"PostFrontendNotifyEvent","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS NOTIFYEVENT","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/FRONTEND/POST_FRONTEND_NOTIFY_EVENT' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'NOTIFYEVENT' ) ON CONFLICT DO NOTHING;
