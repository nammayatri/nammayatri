-- {"api":"GetAccountFetchUnverifiedAccounts","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/ACCOUNT/GET_ACCOUNT_FETCH_UNVERIFIED_ACCOUNTS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostAccountVerifyAccount","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/ACCOUNT/POST_ACCOUNT_VERIFY_ACCOUNT' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PutAccountUpdateRole","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/ACCOUNT/PUT_ACCOUNT_UPDATE_ROLE' ) ON CONFLICT DO NOTHING;
