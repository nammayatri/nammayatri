-- {"api":"GetFeedbackFormList","migration":"capability","param":"system-config.merchant.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.merchant.read', 'DASHBOARD', 'RIDER_MANAGEMENT/FEEDBACK_FORM/GET_FEEDBACK_FORM_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFeedbackFormCreate","migration":"capability","param":"system-config.merchant.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.merchant.write', 'DASHBOARD', 'RIDER_MANAGEMENT/FEEDBACK_FORM/POST_FEEDBACK_FORM_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PutFeedbackFormUpdate","migration":"capability","param":"system-config.merchant.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.merchant.write', 'DASHBOARD', 'RIDER_MANAGEMENT/FEEDBACK_FORM/PUT_FEEDBACK_FORM_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"DeleteFeedbackFormDelete","migration":"capability","param":"system-config.merchant.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.merchant.write', 'DASHBOARD', 'RIDER_MANAGEMENT/FEEDBACK_FORM/DELETE_FEEDBACK_FORM_DELETE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFeedbackForm","migration":"capability","param":"system-config.merchant.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.merchant.read', 'DASHBOARD', 'RIDER_MANAGEMENT/FEEDBACK_FORM/GET_FEEDBACK_FORM' ) ON CONFLICT DO NOTHING;
