-- {"api":"PostUpdatePartnerOrgStationIDUpdatePartnerOrgStationID","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MIGRATION UPDATE_PARTNER_ORG_STATION_ID","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/UPDATE_PARTNER_ORG_STATION_ID/POST_UPDATE_PARTNER_ORG_STATION_ID_UPDATE_PARTNER_ORG_STATION_ID' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MIGRATION' AND T1.user_action_type = 'UPDATE_PARTNER_ORG_STATION_ID' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostUpdatePartnerOrgStationIDUpdatePartnerOrgStationID","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT PARTNER_ORG_ACTION UPDATE_PARTNER_ORG_STATION_ID","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/UPDATE_PARTNER_ORG_STATION_ID/POST_UPDATE_PARTNER_ORG_STATION_ID_UPDATE_PARTNER_ORG_STATION_ID' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'PARTNER_ORG_ACTION' AND T1.user_action_type = 'UPDATE_PARTNER_ORG_STATION_ID' ) ON CONFLICT DO NOTHING;
