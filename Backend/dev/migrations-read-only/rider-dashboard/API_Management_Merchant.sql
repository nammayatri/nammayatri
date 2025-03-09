-- {"api":"PostMerchantUpdate","migration":"endpoint","param":"MerchantAPI MerchantUpdateEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE'
  WHERE endpoint = 'MerchantAPI MerchantUpdateEndpoint';

-- {"api":"PostMerchantUpdate","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantUpdateEndpoint';

-- {"api":"PostMerchantServiceConfigMapsUpdate","migration":"endpoint","param":"MerchantAPI MapsServiceConfigUpdateEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_MAPS_UPDATE'
  WHERE endpoint = 'MerchantAPI MapsServiceConfigUpdateEndpoint';

-- {"api":"PostMerchantServiceConfigMapsUpdate","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_MAPS_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantServiceConfigMapsUpdateEndpoint';

-- {"api":"PostMerchantServiceUsageConfigMapsUpdate","migration":"endpoint","param":"MerchantAPI MapsServiceConfigUsageUpdateEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_MAPS_UPDATE'
  WHERE endpoint = 'MerchantAPI MapsServiceConfigUsageUpdateEndpoint';

-- {"api":"PostMerchantServiceUsageConfigMapsUpdate","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_MAPS_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantServiceUsageConfigMapsUpdateEndpoint';

-- {"api":"PostMerchantServiceConfigSmsUpdate","migration":"endpoint","param":"MerchantAPI SmsServiceConfigUpdateEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_SMS_UPDATE'
  WHERE endpoint = 'MerchantAPI SmsServiceConfigUpdateEndpoint';

-- {"api":"PostMerchantServiceConfigSmsUpdate","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_SMS_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantServiceConfigSmsUpdateEndpoint';

-- {"api":"PostMerchantServiceUsageConfigSmsUpdate","migration":"endpoint","param":"MerchantAPI SmsServiceConfigUsageUpdateEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_SMS_UPDATE'
  WHERE endpoint = 'MerchantAPI SmsServiceConfigUsageUpdateEndpoint';

-- {"api":"PostMerchantServiceUsageConfigSmsUpdate","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_SMS_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantServiceUsageConfigSmsUpdateEndpoint';

-- {"api":"PostMerchantConfigOperatingCityCreate","migration":"endpoint","param":"MerchantAPI CreateMerchantOperatingCityEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_CREATE'
  WHERE endpoint = 'MerchantAPI CreateMerchantOperatingCityEndpoint';

-- {"api":"PostMerchantConfigOperatingCityCreate","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_CREATE'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigOperatingCityCreateEndpoint';

-- {"api":"PostMerchantSpecialLocationUpsert","migration":"endpoint","param":"MerchantAPI UpsertSpecialLocationEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_UPSERT'
  WHERE endpoint = 'MerchantAPI UpsertSpecialLocationEndpoint';

-- {"api":"PostMerchantSpecialLocationUpsert","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_UPSERT'
  WHERE endpoint = 'MerchantAPI PostMerchantSpecialLocationUpsertEndpoint';

-- {"api":"DeleteMerchantSpecialLocationDelete","migration":"endpoint","param":"MerchantAPI DeleteSpecialLocationEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_DELETE'
  WHERE endpoint = 'MerchantAPI DeleteSpecialLocationEndpoint';

-- {"api":"DeleteMerchantSpecialLocationDelete","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_DELETE'
  WHERE endpoint = 'MerchantAPI DeleteMerchantSpecialLocationDeleteEndpoint';

-- {"api":"PostMerchantSpecialLocationGatesUpsert","migration":"endpoint","param":"MerchantAPI UpsertSpecialLocationGateEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_GATES_UPSERT'
  WHERE endpoint = 'MerchantAPI UpsertSpecialLocationGateEndpoint';

-- {"api":"PostMerchantSpecialLocationGatesUpsert","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_GATES_UPSERT'
  WHERE endpoint = 'MerchantAPI PostMerchantSpecialLocationGatesUpsertEndpoint';

-- {"api":"DeleteMerchantSpecialLocationGatesDelete","migration":"endpoint","param":"MerchantAPI DeleteSpecialLocationGateEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_GATES_DELETE'
  WHERE endpoint = 'MerchantAPI DeleteSpecialLocationGateEndpoint';

-- {"api":"DeleteMerchantSpecialLocationGatesDelete","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_GATES_DELETE'
  WHERE endpoint = 'MerchantAPI DeleteMerchantSpecialLocationGatesDeleteEndpoint';


------- SQL updates -------

-- {"api":"PostMerchantUpdate","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MERCHANT MERCHANT_UPDATE","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'MERCHANT_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetMerchantServiceUsageConfig","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MERCHANT SERVICE_USAGE_CONFIG","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_SERVICE_USAGE_CONFIG' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'SERVICE_USAGE_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantServiceConfigMapsUpdate","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MERCHANT MAPS_SERVICE_CONFIG_UPDATE","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_MAPS_UPDATE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'MAPS_SERVICE_CONFIG_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantServiceUsageConfigMapsUpdate","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MERCHANT MAPS_SERVICE_USAGE_CONFIG_UPDATE","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_MAPS_UPDATE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'MAPS_SERVICE_USAGE_CONFIG_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantServiceConfigSmsUpdate","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MERCHANT SMS_SERVICE_CONFIG_UPDATE","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_SMS_UPDATE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'SMS_SERVICE_CONFIG_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantServiceUsageConfigSmsUpdate","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MERCHANT SMS_SERVICE_USAGE_CONFIG_UPDATE","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_SMS_UPDATE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'SMS_SERVICE_USAGE_CONFIG_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantConfigOperatingCityCreate","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MERCHANT CREATE_MERCHANT_OPERATING_CITY","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_CREATE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'CREATE_MERCHANT_OPERATING_CITY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantSpecialLocationUpsert","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MERCHANT UPSERT_SPECIAL_LOCATION","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_UPSERT' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'UPSERT_SPECIAL_LOCATION' ) ON CONFLICT DO NOTHING;

-- {"api":"DeleteMerchantSpecialLocationDelete","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MERCHANT DELETE_SPECIAL_LOCATION","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_DELETE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'DELETE_SPECIAL_LOCATION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantSpecialLocationGatesUpsert","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MERCHANT UPSERT_SPECIAL_LOCATION_GATE","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_GATES_UPSERT' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'UPSERT_SPECIAL_LOCATION_GATE' ) ON CONFLICT DO NOTHING;

-- {"api":"DeleteMerchantSpecialLocationGatesDelete","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MERCHANT DELETE_SPECIAL_LOCATION_GATE","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_GATES_DELETE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'DELETE_SPECIAL_LOCATION_GATE' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostMerchantConfigFailover","migration":"endpoint","param":"MerchantAPI ToggleConfigPriorityEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FAILOVER'
  WHERE endpoint = 'MerchantAPI ToggleConfigPriorityEndpoint';

-- {"api":"PostMerchantConfigFailover","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FAILOVER'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigFailoverEndpoint';

-- {"api":"PostMerchantConfigFailover","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MERCHANT TOGGLE_CONFIG_PRIORITY","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FAILOVER' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'TOGGLE_CONFIG_PRIORITY' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostMerchantTicketConfigUpsert","migration":"endpoint","param":"MerchantAPI UpsertTicketConfigEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_TICKET_CONFIG_UPSERT'
  WHERE endpoint = 'MerchantAPI UpsertTicketConfigEndpoint';

-- {"api":"PostMerchantTicketConfigUpsert","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_TICKET_CONFIG_UPSERT'
  WHERE endpoint = 'MerchantAPI PostMerchantTicketConfigUpsertEndpoint';

-- {"api":"PostMerchantTicketConfigUpsert","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MERCHANT UPSERT_FARE_TICKET_CONFIG","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_TICKET_CONFIG_UPSERT' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'UPSERT_FARE_TICKET_CONFIG' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostMerchantConfigSpecialLocationUpsert","migration":"endpoint","param":"MerchantAPI UpsertSpecialLocationCsvEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_SPECIAL_LOCATION_UPSERT'
  WHERE endpoint = 'MerchantAPI UpsertSpecialLocationCsvEndpoint';

-- {"api":"PostMerchantConfigSpecialLocationUpsert","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_SPECIAL_LOCATION_UPSERT'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigSpecialLocationUpsertEndpoint';

-- {"api":"PostMerchantConfigSpecialLocationUpsert","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT UPSERT_SPECIAL_LOCATION_CSV","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_SPECIAL_LOCATION_UPSERT' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'UPSERT_SPECIAL_LOCATION_CSV' ) ON CONFLICT DO NOTHING;
