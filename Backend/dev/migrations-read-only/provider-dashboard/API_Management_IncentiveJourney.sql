-- {"api":"GetIncentiveJourneyList","migration":"capability","param":"system-config.coins.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIncentiveJourneyCreate","migration":"capability","param":"system-config.coins.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PutIncentiveJourneyUpdate","migration":"capability","param":"system-config.coins.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetIncentiveJourneyMilestoneList","migration":"capability","param":"system-config.coins.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_MILESTONE_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIncentiveJourneyMilestoneCreate","migration":"capability","param":"system-config.coins.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_MILESTONE_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PutIncentiveJourneyMilestoneUpdate","migration":"capability","param":"system-config.coins.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_MILESTONE_UPDATE' ) ON CONFLICT DO NOTHING;
