-- {"api":"GetIssueCategoryList","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT ISSUE ISSUE_CATEGORY_LIST","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CATEGORY_LIST' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'ISSUE' AND T1.user_action_type = 'ISSUE_CATEGORY_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetIssueList","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT ISSUE ISSUE_LIST","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_LIST' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'ISSUE' AND T1.user_action_type = 'ISSUE_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetIssueInfo","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT ISSUE ISSUE_INFO","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_INFO' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'ISSUE' AND T1.user_action_type = 'ISSUE_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"GetIssueInfoV2","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT ISSUE ISSUE_INFO","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_INFO_V2' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'ISSUE' AND T1.user_action_type = 'ISSUE_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"PutIssueUpdate","migration":"endpoint","param":"IssueAPI IssueUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/PUT_ISSUE_UPDATE'
  WHERE endpoint = 'IssueAPI IssueUpdateEndpoint';

-- {"api":"PutIssueUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT ISSUE ISSUE_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/PUT_ISSUE_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'ISSUE' AND T1.user_action_type = 'ISSUE_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIssueComment","migration":"endpoint","param":"IssueAPI IssueAddCommentEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_COMMENT'
  WHERE endpoint = 'IssueAPI IssueAddCommentEndpoint';

-- {"api":"PostIssueComment","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT ISSUE ISSUE_ADD_COMMENT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_COMMENT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'ISSUE' AND T1.user_action_type = 'ISSUE_ADD_COMMENT' ) ON CONFLICT DO NOTHING;

-- {"api":"GetIssueMedia","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT ISSUE ISSUE_FETCH_MEDIA","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_MEDIA' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'ISSUE' AND T1.user_action_type = 'ISSUE_FETCH_MEDIA' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIssueTicketStatusCallBack","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT ISSUE TICKET_STATUS_CALL_BACK","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_TICKET_STATUS_CALL_BACK' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'ISSUE' AND T1.user_action_type = 'TICKET_STATUS_CALL_BACK' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIssueCategoryCreate","migration":"endpoint","param":"IssueAPI CreateIssueCategoryEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_CREATE'
  WHERE endpoint = 'IssueAPI CreateIssueCategoryEndpoint';

-- {"api":"PostIssueCategoryCreate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT ISSUE CREATE_ISSUE_CATEGORY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_CREATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'ISSUE' AND T1.user_action_type = 'CREATE_ISSUE_CATEGORY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIssueCategoryUpdate","migration":"endpoint","param":"IssueAPI UpdateIssueCategoryEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_UPDATE'
  WHERE endpoint = 'IssueAPI UpdateIssueCategoryEndpoint';

-- {"api":"PostIssueCategoryUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT ISSUE UPDATE_ISSUE_CATEGORY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'ISSUE' AND T1.user_action_type = 'UPDATE_ISSUE_CATEGORY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIssueOptionCreate","migration":"endpoint","param":"IssueAPI CreateIssueOptionEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_CREATE'
  WHERE endpoint = 'IssueAPI CreateIssueOptionEndpoint';

-- {"api":"PostIssueOptionCreate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT ISSUE CREATE_ISSUE_OPTION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_CREATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'ISSUE' AND T1.user_action_type = 'CREATE_ISSUE_OPTION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIssueOptionUpdate","migration":"endpoint","param":"IssueAPI UpdateIssueOptionEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_UPDATE'
  WHERE endpoint = 'IssueAPI UpdateIssueOptionEndpoint';

-- {"api":"PostIssueOptionUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT ISSUE UPDATE_ISSUE_OPTION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'ISSUE' AND T1.user_action_type = 'UPDATE_ISSUE_OPTION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIssueMessageUpsert","migration":"endpoint","param":"IssueAPI UpsertIssueMessageEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_MESSAGE_UPSERT'
  WHERE endpoint = 'IssueAPI UpsertIssueMessageEndpoint';

-- {"api":"PostIssueMessageUpsert","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT ISSUE UPSERT_ISSUE_MESSAGE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_MESSAGE_UPSERT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'ISSUE' AND T1.user_action_type = 'UPSERT_ISSUE_MESSAGE' ) ON CONFLICT DO NOTHING;
