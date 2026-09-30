-- {"api":"GetRideFeedbackConfigList","migration":"capability","param":"system-config.ride_feedback.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.read', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_CONFIG_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRideFeedbackConfig","migration":"capability","param":"system-config.ride_feedback.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.read', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideFeedbackConfigCreate","migration":"capability","param":"system-config.ride_feedback.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.write', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_CONFIG_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideFeedbackConfigUpdate","migration":"capability","param":"system-config.ride_feedback.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.write', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_CONFIG_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideFeedbackConfigToggle","migration":"capability","param":"system-config.ride_feedback.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.write', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_CONFIG_TOGGLE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideFeedbackConfigClone","migration":"capability","param":"system-config.ride_feedback.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.write', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_CONFIG_CLONE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideFeedbackConfigValidate","migration":"capability","param":"system-config.ride_feedback.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.write', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_CONFIG_VALIDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRideFeedbackRidePreview","migration":"capability","param":"system-config.ride_feedback.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.read', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_RIDE_PREVIEW' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRideFeedbackRideResponses","migration":"capability","param":"system-config.ride_feedback.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.read', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_RIDE_RESPONSES' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideFeedbackResponseRetryActions","migration":"capability","param":"system-config.ride_feedback.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.write', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_RESPONSE_RETRY_ACTIONS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRideFeedbackMeta","migration":"capability","param":"system-config.ride_feedback.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.read', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_META' ) ON CONFLICT DO NOTHING;
