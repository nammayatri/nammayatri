-- {"api":"PostNammaTagTagCreate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_CREATE'
  WHERE endpoint = 'NammaTagAPI PostNammaTagTagCreateEndpoint';

-- {"api":"PostNammaTagQueryCreate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_QUERY_CREATE'
  WHERE endpoint = 'NammaTagAPI PostNammaTagQueryCreateEndpoint';

-- {"api":"PostNammaTagAppDynamicLogicVerify","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERIFY'
  WHERE endpoint = 'NammaTagAPI PostNammaTagAppDynamicLogicVerifyEndpoint';


------- SQL updates -------

-- {"api":"PostNammaTagTagCreate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG CREATE_NAMMA_TAG","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_CREATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'CREATE_NAMMA_TAG' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagTagUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG CREATE_NAMMA_TAG","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'CREATE_NAMMA_TAG' ) ON CONFLICT DO NOTHING;

-- {"api":"DeleteNammaTagTagDelete","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG CREATE_NAMMA_TAG","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/DELETE_NAMMA_TAG_TAG_DELETE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'CREATE_NAMMA_TAG' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagQueryCreate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG CREATE_CHAKRA_QUERY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_QUERY_CREATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'CREATE_CHAKRA_QUERY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagAppDynamicLogicVerify","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG APP_DYNAMIC_LOGIC_VERIFY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERIFY' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'APP_DYNAMIC_LOGIC_VERIFY' ) ON CONFLICT DO NOTHING;

-- {"api":"GetNammaTagAppDynamicLogic","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG APP_DYNAMIC_LOGIC_VERIFY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'APP_DYNAMIC_LOGIC_VERIFY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagRunJob","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG RUN_KAAL_CHAKRA_JOB","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_RUN_JOB' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'RUN_KAAL_CHAKRA_JOB' ) ON CONFLICT DO NOTHING;

-- {"api":"GetNammaTagTimeBounds","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG TIME_BOUNDS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_TIME_BOUNDS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'TIME_BOUNDS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagTimeBoundsCreate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG TIME_BOUNDS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TIME_BOUNDS_CREATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'TIME_BOUNDS' ) ON CONFLICT DO NOTHING;

-- {"api":"DeleteNammaTagTimeBoundsDelete","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG TIME_BOUNDS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/DELETE_NAMMA_TAG_TIME_BOUNDS_DELETE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'TIME_BOUNDS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetNammaTagAppDynamicLogicGetLogicRollout","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG APP_DYNAMIC_LOGIC_ROLLOUT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_GET_LOGIC_ROLLOUT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'APP_DYNAMIC_LOGIC_ROLLOUT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagAppDynamicLogicUpsertLogicRollout","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG APP_DYNAMIC_LOGIC_ROLLOUT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_UPSERT_LOGIC_ROLLOUT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'APP_DYNAMIC_LOGIC_ROLLOUT' ) ON CONFLICT DO NOTHING;

-- {"api":"GetNammaTagAppDynamicLogicVersions","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG APP_DYNAMIC_LOGIC_VERIFY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERSIONS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'APP_DYNAMIC_LOGIC_VERIFY' ) ON CONFLICT DO NOTHING;

-- {"api":"GetNammaTagAppDynamicLogicDomains","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG APP_DYNAMIC_LOGIC_VERIFY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_DOMAINS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'APP_DYNAMIC_LOGIC_VERIFY' ) ON CONFLICT DO NOTHING;

-- {"api":"GetNammaTagQueryAll","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG GET_CHAKRA_QUERY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_QUERY_ALL' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'GET_CHAKRA_QUERY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagTagVerify","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG CREATE_NAMMA_TAG","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_VERIFY' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'CREATE_NAMMA_TAG' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostNammaTagConfigPilotGetVersion","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG POST_RETRIEVE_VERSION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_VERSION' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'POST_RETRIEVE_VERSION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagConfigPilotGetConfig","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG POST_RETRIEVE_CONFIG","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_CONFIG' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'POST_RETRIEVE_CONFIG' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostNammaTagConfigPilotCreateUiConfig","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG CREATE_UI_CONFIG","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_CREATE_UI_CONFIG' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'CREATE_UI_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"GetNammaTagConfigPilotAllConfigs","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG APP_DYNAMIC_LOGIC_VERIFY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_ALL_CONFIGS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'APP_DYNAMIC_LOGIC_VERIFY' ) ON CONFLICT DO NOTHING;

-- {"api":"GetNammaTagConfigPilotConfigDetails","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG APP_DYNAMIC_LOGIC_VERIFY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_CONFIG_DETAILS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'APP_DYNAMIC_LOGIC_VERIFY' ) ON CONFLICT DO NOTHING;

-- {"api":"GetNammaTagConfigPilotGetTableData","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG APP_DYNAMIC_LOGIC_VERIFY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_TABLE_DATA' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'APP_DYNAMIC_LOGIC_VERIFY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagConfigPilotConcludeOrAbortOrRevert","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_CONCLUDE_OR_ABORT_OR_REVERT'
  WHERE endpoint = 'NammaTagAPI PostNammaTagConfigPilotConcludeOrAbortOrRevertEndpoint';

-- {"api":"PostNammaTagConfigPilotConcludeOrAbortOrRevert","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG APP_DYNAMIC_LOGIC_VERIFY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_CONCLUDE_OR_ABORT_OR_REVERT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'APP_DYNAMIC_LOGIC_VERIFY' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostNammaTagConfigPilotActionChange","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_ACTION_CHANGE'
  WHERE endpoint = 'NammaTagAPI PostNammaTagConfigPilotActionChangeEndpoint';

-- {"api":"PostNammaTagConfigPilotActionChange","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT NAMMA_TAG APP_DYNAMIC_LOGIC_VERIFY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_ACTION_CHANGE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'NAMMA_TAG' AND T1.user_action_type = 'APP_DYNAMIC_LOGIC_VERIFY' ) ON CONFLICT DO NOTHING;
