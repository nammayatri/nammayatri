-- {"api":"GetEventManagementTicketdashboardTicketplaceDef","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/GET_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_DEF' ) ON CONFLICT DO NOTHING;

-- {"api":"PostEventManagementTicketdashboardTicketplaceCleardraft","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_CLEARDRAFT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostEventManagementTicketdashboardTicketplaceSubmitDraft","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_SUBMIT_DRAFT' ) ON CONFLICT DO NOTHING;


-- {"api":"PostEventManagementTicketdashboardTicketplaceCreate","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostEventManagementTicketdashboardTicketplaceUpdateBasicInfo","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_UPDATE_BASIC_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"PostEventManagementTicketdashboardTicketplaceUpdateService","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_UPDATE_SERVICE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostEventManagementTicketdashboardTicketplaceDelService","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_DEL_SERVICE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostEventManagementTicketdashboardTicketplaceServiceUpdateCategory","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_SERVICE_UPDATE_CATEGORY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostEventManagementTicketdashboardTicketplaceServiceDelCategory","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_SERVICE_DEL_CATEGORY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostEventManagementTicketdashboardTicketPlaceServiceCategoryUpdatePeople","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
-- INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_SERVICE_CATEGORY_UPDATE_PEOPLE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostEventManagementTicketdashboardTicketPlaceServiceCategoryDelPeople","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
-- INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_SERVICE_CATEGORY_DEL_PEOPLE' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostEventManagementTicketdashboardTicketPlaceCategoryUpdatePeople","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_CATEGORY_UPDATE_PEOPLE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostEventManagementTicketdashboardTicketPlaceCategoryDelPeople","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_CATEGORY_DEL_PEOPLE' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostEventManagementTicketdashboardTicketplaceCancelSubmitDraft","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_CANCEL_SUBMIT_DRAFT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostEventManagementTicketdashboardTicketplaceReviewDraft","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_REVIEW_DRAFT' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetEventManagementTicketdashboardTicketplaceDrafts","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/GET_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_DRAFTS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostEventManagementTicketdashboardTicketplaceRecommend","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_RECOMMEND' ) ON CONFLICT DO NOTHING;
