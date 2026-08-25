-- {"api":"GetOnboardingConfigGet","migration":"capability","param":"city-config.onboarding.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.onboarding.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/ONBOARDING_CONFIG/GET_ONBOARDING_CONFIG_GET' ) ON CONFLICT DO NOTHING;

-- {"api":"PostOnboardingConfigClone","migration":"capability","param":"city-config.onboarding.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.onboarding.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/ONBOARDING_CONFIG/POST_ONBOARDING_CONFIG_CLONE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostOnboardingConfigApply","migration":"capability","param":"city-config.onboarding.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/ONBOARDING_CONFIG/POST_ONBOARDING_CONFIG_APPLY' ) ON CONFLICT DO NOTHING;
