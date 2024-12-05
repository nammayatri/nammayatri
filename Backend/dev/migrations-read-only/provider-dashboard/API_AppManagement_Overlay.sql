-- {"api":"PostOverlayCreate","migration":"endpoint","param":"OverlayAPI CreateOverlayEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/OVERLAY/POST_OVERLAY_CREATE'
  WHERE endpoint = 'OverlayAPI CreateOverlayEndpoint';

-- {"api":"PostOverlayCreate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT OVERLAY CREATE_OVERLAY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/OVERLAY/POST_OVERLAY_CREATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'OVERLAY' AND T1.user_action_type = 'CREATE_OVERLAY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostOverlayDelete","migration":"endpoint","param":"OverlayAPI DeleteOverlayEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/OVERLAY/POST_OVERLAY_DELETE'
  WHERE endpoint = 'OverlayAPI DeleteOverlayEndpoint';

-- {"api":"PostOverlayDelete","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT OVERLAY DELETE_OVERLAY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/OVERLAY/POST_OVERLAY_DELETE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'OVERLAY' AND T1.user_action_type = 'DELETE_OVERLAY' ) ON CONFLICT DO NOTHING;

-- {"api":"GetOverlayList","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT OVERLAY LIST_OVERLAY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/OVERLAY/GET_OVERLAY_LIST' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'OVERLAY' AND T1.user_action_type = 'LIST_OVERLAY' ) ON CONFLICT DO NOTHING;

-- {"api":"GetOverlayInfo","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT OVERLAY OVERLAY_INFO","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/OVERLAY/GET_OVERLAY_INFO' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'OVERLAY' AND T1.user_action_type = 'OVERLAY_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"PostOverlaySchedule","migration":"endpoint","param":"OverlayAPI ScheduleOverlayEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_APP_MANAGEMENT/OVERLAY/POST_OVERLAY_SCHEDULE'
  WHERE endpoint = 'OverlayAPI ScheduleOverlayEndpoint';

-- {"api":"PostOverlaySchedule","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT OVERLAY SCHEDULE_OVERLAY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_APP_MANAGEMENT/OVERLAY/POST_OVERLAY_SCHEDULE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'OVERLAY' AND T1.user_action_type = 'SCHEDULE_OVERLAY' ) ON CONFLICT DO NOTHING;
