-- {"api":"GetPaymentRefundRequestList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/PAYMENT/GET_PAYMENT_REFUND_REQUEST_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPaymentRefundRequestRespond","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/PAYMENT/POST_PAYMENT_REFUND_REQUEST_RESPOND' ) ON CONFLICT DO NOTHING;

-- {"api":"GetPaymentRefundRequestInfo","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/PAYMENT/GET_PAYMENT_REFUND_REQUEST_INFO' ) ON CONFLICT DO NOTHING;
