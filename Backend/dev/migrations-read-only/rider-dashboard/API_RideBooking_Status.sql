-- {"api":"PostConfirmRideSearchQuote","migration":"endpoint","param":"FRFSStatusAPI FRFSStatusEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/STATUS/POST_CONFIRM_RIDE_SEARCH_QUOTE'
  WHERE endpoint = 'FRFSStatusAPI FRFSStatusEndPoint';

-- {"api":"PostConfirmRideSearchQuote","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS STATUS","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/STATUS/POST_CONFIRM_RIDE_SEARCH_QUOTE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'STATUS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostFRFSTicketStatus","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS STATUS","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/STATUS/POST_FRFS_TICKET_STATUS' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'STATUS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostStatusGetFRFSTicketStatus","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS STATUS","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/STATUS/POST_STATUS_GET_FRFS_TICKET_STATUS' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'STATUS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetStatusGetFRFSTicketStatus","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS TICKET_STATUS","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/STATUS/GET_STATUS_GET_FRFS_TICKET_STATUS' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'TICKET_STATUS' ) ON CONFLICT DO NOTHING;
