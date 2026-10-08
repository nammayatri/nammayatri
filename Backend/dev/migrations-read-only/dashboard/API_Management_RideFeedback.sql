-- {"api":"GetRideFeedbackInspectPreview","migration":"capability","param":"system-config.ride_feedback.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_INSPECT_PREVIEW' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRideFeedbackInspectResponses","migration":"capability","param":"system-config.ride_feedback.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_INSPECT_RESPONSES' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRideFeedbackMeta","migration":"capability","param":"system-config.ride_feedback.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_META' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostRideFeedbackBookingResponseRetryActions","migration":"capability","param":"system-config.ride_feedback.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.ride_feedback.write', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_BOOKING_RESPONSE_RETRY_ACTIONS' ) ON CONFLICT DO NOTHING;
