-- {"api":"PostInsertPartnerOrgStationInsertPartnerOrgStation","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MIGRATION INSERT_PARTNER_ORG_STATION","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/INSERT_PARTNER_ORG_STATION/POST_INSERT_PARTNER_ORG_STATION_INSERT_PARTNER_ORG_STATION' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MIGRATION' AND T1.user_action_type = 'INSERT_PARTNER_ORG_STATION' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostInsertPartnerOrgStationInsertPartnerOrgStation","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT PARTNER_ORG_ACTION INSERT_PARTNER_ORG_STATION","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/INSERT_PARTNER_ORG_STATION/POST_INSERT_PARTNER_ORG_STATION_INSERT_PARTNER_ORG_STATION' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'PARTNER_ORG_ACTION' AND T1.user_action_type = 'INSERT_PARTNER_ORG_STATION' ) ON CONFLICT DO NOTHING;
