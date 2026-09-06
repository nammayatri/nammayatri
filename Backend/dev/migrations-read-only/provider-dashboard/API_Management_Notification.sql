-- {"api":"GetNotificationNotificationList","migration":"capability","param":"communication.notification.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'communication.notification.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NOTIFICATION/GET_NOTIFICATION_NOTIFICATION_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNotificationNotificationRespond","migration":"capability","param":"communication.notification.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'communication.notification.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NOTIFICATION/POST_NOTIFICATION_NOTIFICATION_RESPOND' ) ON CONFLICT DO NOTHING;
