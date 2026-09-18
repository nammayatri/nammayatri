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
