-- {"api":"GetAppFeedbackMeta","migration":"capability","param":"system-config.ride_feedback.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.read', 'DASHBOARD', 'RIDER_MANAGEMENT/APP_FEEDBACK/GET_APP_FEEDBACK_META' ) ON CONFLICT DO NOTHING;

-- {"api":"GetAppFeedbackInspectPreview","migration":"capability","param":"system-config.ride_feedback.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.read', 'DASHBOARD', 'RIDER_MANAGEMENT/APP_FEEDBACK/GET_APP_FEEDBACK_INSPECT_PREVIEW' ) ON CONFLICT DO NOTHING;

-- {"api":"GetAppFeedbackInspectResponses","migration":"capability","param":"system-config.ride_feedback.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.read', 'DASHBOARD', 'RIDER_MANAGEMENT/APP_FEEDBACK/GET_APP_FEEDBACK_INSPECT_RESPONSES' ) ON CONFLICT DO NOTHING;

-- {"api":"PostAppFeedbackResponseRetryActions","migration":"capability","param":"system-config.ride_feedback.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.write', 'DASHBOARD', 'RIDER_MANAGEMENT/APP_FEEDBACK/POST_APP_FEEDBACK_RESPONSE_RETRY_ACTIONS' ) ON CONFLICT DO NOTHING;
