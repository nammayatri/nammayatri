-- {"api":"GetFarePolicyV2List","migration":"capability","param":"system-config.fare_policy.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.fare_policy.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FARE_POLICY_V2/GET_FARE_POLICY_V2_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFarePolicyV2Policy","migration":"capability","param":"system-config.fare_policy.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.fare_policy.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FARE_POLICY_V2/GET_FARE_POLICY_V2_POLICY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFarePolicyV2PolicyReplace","migration":"capability","param":"system-config.fare_policy.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.fare_policy.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_POLICY_REPLACE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFarePolicyV2BulkReplace","migration":"capability","param":"system-config.fare_policy.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.fare_policy.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_BULK_REPLACE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFarePolicyV2Preview","migration":"capability","param":"system-config.fare_policy.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.fare_policy.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_PREVIEW' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFarePolicyV2ProductCreate","migration":"capability","param":"system-config.fare_policy.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.fare_policy.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_PRODUCT_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFarePolicyV2ProductUpdate","migration":"capability","param":"system-config.fare_policy.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.fare_policy.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_PRODUCT_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFarePolicyV2ProductRemove","migration":"capability","param":"system-config.fare_policy.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.fare_policy.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_PRODUCT_REMOVE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFarePolicyV2ChangeRequestList","migration":"capability","param":"system-config.fare_policy.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.fare_policy.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FARE_POLICY_V2/GET_FARE_POLICY_V2_CHANGE_REQUEST_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFarePolicyV2ChangeRequestDecide","migration":"capability","param":"system-config.fare_policy.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.fare_policy.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_CHANGE_REQUEST_DECIDE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFarePolicyV2AlertsSubscriptions","migration":"capability","param":"system-config.fare_policy.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.fare_policy.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FARE_POLICY_V2/GET_FARE_POLICY_V2_ALERTS_SUBSCRIPTIONS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFarePolicyV2AlertsSubscribe","migration":"capability","param":"system-config.fare_policy.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.fare_policy.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_ALERTS_SUBSCRIBE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFarePolicyV2AlertsUnsubscribe","migration":"capability","param":"system-config.fare_policy.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.fare_policy.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_ALERTS_UNSUBSCRIBE' ) ON CONFLICT DO NOTHING;
