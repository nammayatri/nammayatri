-- {"api":"PostConfirmRideSearchQuote","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/STATUS/POST_CONFIRM_RIDE_SEARCH_QUOTE' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostFRFSTicketStatus","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/STATUS/POST_FRFS_TICKET_STATUS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostStatusGetFRFSTicketStatus","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/STATUS/POST_STATUS_GET_FRFS_TICKET_STATUS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetStatusGetFRFSTicketStatus","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/STATUS/GET_STATUS_GET_FRFS_TICKET_STATUS' ) ON CONFLICT DO NOTHING;
