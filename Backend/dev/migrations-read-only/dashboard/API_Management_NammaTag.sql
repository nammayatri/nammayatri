-- {"api":"PostNammaTagTagCreate","migration":"endpointV2","param":null,"schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_CREATE'
  WHERE endpoint = 'NammaTagAPI PostNammaTagTagCreateEndpoint';

-- {"api":"PostNammaTagQueryCreate","migration":"endpointV2","param":null,"schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_QUERY_CREATE'
  WHERE endpoint = 'NammaTagAPI PostNammaTagQueryCreateEndpoint';

-- {"api":"PostNammaTagAppDynamicLogicVerify","migration":"endpointV2","param":null,"schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERIFY'
  WHERE endpoint = 'NammaTagAPI PostNammaTagAppDynamicLogicVerifyEndpoint';

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_CONCLUDE_OR_ABORT_OR_REVERT'
  WHERE endpoint = 'NammaTagAPI PostNammaTagConfigPilotConcludeOrAbortOrRevertEndpoint';

------- SQL updates -------

-- {"api":"PostNammaTagConfigPilotActionChange","migration":"endpointV2","param":null,"schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_ACTION_CHANGE'
  WHERE endpoint = 'NammaTagAPI PostNammaTagConfigPilotActionChangeEndpoint';

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

-- {"api":"PostNammaTagAppDynamicLogicUpdateExperimentGroup","migration":"capability","param":"system-config.dynamic_logic.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_UPDATE_EXPERIMENT_GROUP' ) ON CONFLICT DO NOTHING;

------- SQL updates -------

-- {"api":"GetNammaTagAppDynamicLogicExperimentGroups","migration":"capability","param":"system-config.dynamic_logic.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_EXPERIMENT_GROUPS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostNammaTagBulkUpdateCustomerTag","migration":"capability","param":"system-config.namma_tag.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.namma_tag.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_BULK_UPDATE_CUSTOMER_TAG' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostNammaTagAppDynamicLogicBulkUpsertLogicRollout","migration":"capability","param":"system-config.dynamic_logic.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_BULK_UPSERT_LOGIC_ROLLOUT' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostNammaTagConfigPilotVerify","migration":"capability","param":"system-config.config_pilot.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.config_pilot.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_VERIFY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagConfigPilotUpsertLogicRollout","migration":"capability","param":"system-config.config_pilot.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.config_pilot.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_UPSERT_LOGIC_ROLLOUT' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostNammaTagConfigPilotRolloutAction","migration":"capability","param":"system-config.config_pilot.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.config_pilot.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_ROLLOUT_ACTION' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetNammaTagBehaviorStatus","migration":"capability","param":"city-operations.behaviour.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.behaviour.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_BEHAVIOR_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagBehaviorEnable","migration":"capability","param":"city-operations.behaviour.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.behaviour.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_BEHAVIOR_ENABLE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagBehaviorDisable","migration":"capability","param":"city-operations.behaviour.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.behaviour.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_BEHAVIOR_DISABLE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagBehaviorMarkCanonical","migration":"capability","param":"system-config.dynamic_logic.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.dynamic_logic.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_BEHAVIOR_MARK_CANONICAL' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetNammaTagConfigPilotGetTableDataFromDB","migration":"capability","param":"system-config.config_pilot.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.config_pilot.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_TABLE_DATA_FROM_DB' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetNammaTagConfigPilotGetTableDataFromDB","migration":"capability","param":"system-config.config_pilot.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.config_pilot.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_TABLE_DATA_FROM_DB' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetNammaTagChakraRuns","migration":"capability","param":"system-config.kaal_chakra.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.kaal_chakra.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CHAKRA_RUNS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetNammaTagChakraRun","migration":"capability","param":"system-config.kaal_chakra.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.kaal_chakra.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CHAKRA_RUN' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagChakraJobsReschedule","migration":"capability","param":"system-config.kaal_chakra.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.kaal_chakra.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CHAKRA_JOBS_RESCHEDULE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagChakraJobsCancel","migration":"capability","param":"system-config.kaal_chakra.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.kaal_chakra.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CHAKRA_JOBS_CANCEL' ) ON CONFLICT DO NOTHING;

-- {"api":"GetNammaTagChakraRuns","migration":"capability","param":"system-config.kaal_chakra.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.kaal_chakra.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CHAKRA_RUNS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetNammaTagChakraRun","migration":"capability","param":"system-config.kaal_chakra.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.kaal_chakra.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CHAKRA_RUN' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagChakraJobsReschedule","migration":"capability","param":"system-config.kaal_chakra.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.kaal_chakra.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CHAKRA_JOBS_RESCHEDULE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagChakraJobsCancel","migration":"capability","param":"system-config.kaal_chakra.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.kaal_chakra.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CHAKRA_JOBS_CANCEL' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostNammaTagChakraJobsDelete","migration":"capability","param":"system-config.kaal_chakra.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.kaal_chakra.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CHAKRA_JOBS_DELETE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetNammaTagChakraJobsUpcoming","migration":"capability","param":"system-config.kaal_chakra.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.kaal_chakra.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CHAKRA_JOBS_UPCOMING' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagChakraJobsDelete","migration":"capability","param":"system-config.kaal_chakra.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.kaal_chakra.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CHAKRA_JOBS_DELETE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetNammaTagChakraJobsUpcoming","migration":"capability","param":"system-config.kaal_chakra.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.kaal_chakra.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CHAKRA_JOBS_UPCOMING' ) ON CONFLICT DO NOTHING;
