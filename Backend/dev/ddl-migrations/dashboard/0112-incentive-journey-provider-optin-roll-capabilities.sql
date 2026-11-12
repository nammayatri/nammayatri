

-- {"api":"PostIncentiveJourneyOptIn","migration":"capability","param":"system-config.coins.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id)
VALUES (
  'system-config.coins.write',
  'DASHBOARD',
  'PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_OPT_IN'
) ON CONFLICT DO NOTHING;

-- {"api":"PostIncentiveJourneyCohortJourneyRoll","migration":"capability","param":"system-config.coins.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id)
VALUES (
  'system-config.coins.write',
  'DASHBOARD',
  'PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_COHORT_JOURNEY_ROLL'
) ON CONFLICT DO NOTHING;
