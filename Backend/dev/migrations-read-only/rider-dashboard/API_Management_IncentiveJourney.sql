-- {"api":"GetIncentiveJourneyStatsHistory","migration":"capability","param":"system-config.coins.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.read', 'DASHBOARD', 'RIDER_MANAGEMENT/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_STATS_HISTORY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIncentiveJourneyStatsWaiveOff","migration":"capability","param":"system-config.coins.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'RIDER_MANAGEMENT/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_STATS_WAIVE_OFF' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostIncentiveJourneyAssign","migration":"capability","param":"system-config.coins.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'RIDER_MANAGEMENT/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_ASSIGN' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetIncentiveJourneyList","migration":"capability","param":"system-config.coins.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.read', 'DASHBOARD', 'RIDER_MANAGEMENT/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIncentiveJourneyCreate","migration":"capability","param":"system-config.coins.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'RIDER_MANAGEMENT/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PutIncentiveJourneyUpdate","migration":"capability","param":"system-config.coins.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'RIDER_MANAGEMENT/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetIncentiveJourneyMilestoneList","migration":"capability","param":"system-config.coins.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.read', 'DASHBOARD', 'RIDER_MANAGEMENT/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_MILESTONE_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIncentiveJourneyMilestoneCreate","migration":"capability","param":"system-config.coins.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'RIDER_MANAGEMENT/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_MILESTONE_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PutIncentiveJourneyMilestoneUpdate","migration":"capability","param":"system-config.coins.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'RIDER_MANAGEMENT/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_MILESTONE_UPDATE' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"DeleteIncentiveJourneyUnassign","migration":"capability","param":"system-config.coins.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'RIDER_MANAGEMENT/INCENTIVE_JOURNEY/DELETE_INCENTIVE_JOURNEY_UNASSIGN' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostIncentiveJourneyCohortCreate","migration":"capability","param":"system-config.coins.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'RIDER_MANAGEMENT/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_COHORT_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIncentiveJourneyCohortJourneyCreate","migration":"capability","param":"system-config.coins.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'RIDER_MANAGEMENT/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_COHORT_JOURNEY_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PutIncentiveJourneyCohortJourneyUpdate","migration":"capability","param":"system-config.coins.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.coins.write', 'DASHBOARD', 'RIDER_MANAGEMENT/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_COHORT_JOURNEY_UPDATE' ) ON CONFLICT DO NOTHING;
