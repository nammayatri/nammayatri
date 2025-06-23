-- {"api":"TicketDashboardUploadAsset","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_UPLOAD_ASSET' ) ON CONFLICT DO NOTHING;

-- {"api":"TicketDashboardDeleteAsset","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_DELETE_ASSET' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"TicketDashboardCurrentSeatStatus","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_CURRENT_SEAT_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"TicketDashboardSeatManagement","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_SEAT_MANAGEMENT' ) ON CONFLICT DO NOTHING;
