-- {"api":"PostCommunicationCreate","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/COMMUNICATION/POST_COMMUNICATION_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetCommunicationList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/COMMUNICATION/GET_COMMUNICATION_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetCommunicationInfo","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/COMMUNICATION/GET_COMMUNICATION_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"PostCommunicationSend","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/COMMUNICATION/POST_COMMUNICATION_SEND' ) ON CONFLICT DO NOTHING;

-- {"api":"PutCommunicationEdit","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/COMMUNICATION/PUT_COMMUNICATION_EDIT' ) ON CONFLICT DO NOTHING;

-- {"api":"DeleteCommunicationDelete","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/COMMUNICATION/DELETE_COMMUNICATION_DELETE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetCommunicationDeliveryStatus","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/COMMUNICATION/GET_COMMUNICATION_DELIVERY_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostCommunicationUpload","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/COMMUNICATION/POST_COMMUNICATION_UPLOAD' ) ON CONFLICT DO NOTHING;

-- {"api":"PostCommunicationAddMediaLink","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/COMMUNICATION/POST_COMMUNICATION_ADD_MEDIA_LINK' ) ON CONFLICT DO NOTHING;

-- {"api":"GetCommunicationRecipients","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/COMMUNICATION/GET_COMMUNICATION_RECIPIENTS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetCommunicationTemplate","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/COMMUNICATION/GET_COMMUNICATION_TEMPLATE' ) ON CONFLICT DO NOTHING;
