-- {"api":"PostSearchRequestSearchrequests","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/SEARCH_REQUEST/POST_SEARCH_REQUEST_SEARCHREQUESTS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetSearchRequestList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/SEARCH_REQUEST/GET_SEARCH_REQUEST_LIST' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetSearchRequestInfo","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/SEARCH_REQUEST/GET_SEARCH_REQUEST_INFO' ) ON CONFLICT DO NOTHING;
