-- {"api":"GetRideFeedbackRidePreview","migration":"capability","param":"system-config.ride_feedback.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_RIDE_PREVIEW' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRideFeedbackRideResponses","migration":"capability","param":"system-config.ride_feedback.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_RIDE_RESPONSES' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRideFeedbackMeta","migration":"capability","param":"system-config.ride_feedback.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_META' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostRideFeedbackRideResponseRetryActions","migration":"capability","param":"system-config.ride_feedback.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.write', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_RIDE_RESPONSE_RETRY_ACTIONS' ) ON CONFLICT DO NOTHING;
