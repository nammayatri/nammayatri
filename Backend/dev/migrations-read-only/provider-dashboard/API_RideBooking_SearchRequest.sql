-- {"api":"GetSearchRequestSearchrequests","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SEARCH_REQUESTS LIST_SEARCH_REQUESTS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/SEARCH_REQUEST/GET_SEARCH_REQUEST_SEARCHREQUESTS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SEARCH_REQUESTS' AND T1.user_action_type = 'LIST_SEARCH_REQUESTS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetSearchRequestList","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT SEARCH_REQUESTS LIST_SEARCH_REQUESTS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/SEARCH_REQUEST/GET_SEARCH_REQUEST_LIST' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'SEARCH_REQUESTS' AND T1.user_action_type = 'LIST_SEARCH_REQUESTS' ) ON CONFLICT DO NOTHING;
