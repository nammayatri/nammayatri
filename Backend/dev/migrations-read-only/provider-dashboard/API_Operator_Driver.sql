-- {"api":"GetDriverOperatorFetchHubRequests","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT OPERATOR GET_HUB_REQUESTS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_OPERATOR/DRIVER/GET_DRIVER_OPERATOR_FETCH_HUB_REQUESTS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'OPERATOR' AND T1.user_action_type = 'GET_HUB_REQUESTS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostDriverOperatorRespondHubRequest","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT OPERATOR RESPOND_HUB_REQUEST","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_OPERATOR/DRIVER/POST_DRIVER_OPERATOR_RESPOND_HUB_REQUEST' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'OPERATOR' AND T1.user_action_type = 'RESPOND_HUB_REQUEST' ) ON CONFLICT DO NOTHING;
