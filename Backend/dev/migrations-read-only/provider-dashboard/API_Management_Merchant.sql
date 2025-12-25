-- {"api":"PostMerchantUpdate","migration":"endpoint","param":"MerchantAPI MerchantUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE'
  WHERE endpoint = 'MerchantAPI MerchantUpdateEndpoint';

-- {"api":"PostMerchantUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantUpdateEndpoint';

-- {"api":"PostMerchantConfigCommonUpdate","migration":"endpoint","param":"MerchantAPI MerchantCommonConfigUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_COMMON_UPDATE'
  WHERE endpoint = 'MerchantAPI MerchantCommonConfigUpdateEndpoint';

-- {"api":"PostMerchantConfigCommonUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_COMMON_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigCommonUpdateEndpoint';

-- {"api":"PostMerchantConfigDriverPoolUpdate","migration":"endpoint","param":"MerchantAPI DriverPoolConfigUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_POOL_UPDATE'
  WHERE endpoint = 'MerchantAPI DriverPoolConfigUpdateEndpoint';

-- {"api":"PostMerchantConfigDriverPoolUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_POOL_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigDriverPoolUpdateEndpoint';

-- {"api":"PostMerchantConfigDriverPoolCreate","migration":"endpoint","param":"MerchantAPI DriverPoolConfigCreateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_POOL_CREATE'
  WHERE endpoint = 'MerchantAPI DriverPoolConfigCreateEndpoint';

-- {"api":"PostMerchantConfigDriverPoolCreate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_POOL_CREATE'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigDriverPoolCreateEndpoint';

-- {"api":"PostMerchantConfigDriverIntelligentPoolUpdate","migration":"endpoint","param":"MerchantAPI DriverIntelligentPoolConfigUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_INTELLIGENT_POOL_UPDATE'
  WHERE endpoint = 'MerchantAPI DriverIntelligentPoolConfigUpdateEndpoint';

-- {"api":"PostMerchantConfigDriverIntelligentPoolUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_INTELLIGENT_POOL_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigDriverIntelligentPoolUpdateEndpoint';

-- {"api":"PostMerchantConfigOnboardingDocumentUpdate","migration":"endpoint","param":"MerchantAPI DocumentVerificationConfigUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_ONBOARDING_DOCUMENT_UPDATE'
  WHERE endpoint = 'MerchantAPI DocumentVerificationConfigUpdateEndpoint';

-- {"api":"PostMerchantConfigOnboardingDocumentUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_ONBOARDING_DOCUMENT_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigOnboardingDocumentUpdateEndpoint';

-- {"api":"PostMerchantConfigOnboardingDocumentCreate","migration":"endpoint","param":"MerchantAPI DocumentVerificationConfigCreateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_ONBOARDING_DOCUMENT_CREATE'
  WHERE endpoint = 'MerchantAPI DocumentVerificationConfigCreateEndpoint';

-- {"api":"PostMerchantConfigOnboardingDocumentCreate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_ONBOARDING_DOCUMENT_CREATE'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigOnboardingDocumentCreateEndpoint';

-- {"api":"PostMerchantServiceConfigMapsUpdate","migration":"endpoint","param":"MerchantAPI MapsServiceConfigUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_MAPS_UPDATE'
  WHERE endpoint = 'MerchantAPI MapsServiceConfigUpdateEndpoint';

-- {"api":"PostMerchantServiceConfigMapsUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_MAPS_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantServiceConfigMapsUpdateEndpoint';

-- {"api":"PostMerchantServiceUsageConfigMapsUpdate","migration":"endpoint","param":"MerchantAPI MapsServiceConfigUsageUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_MAPS_UPDATE'
  WHERE endpoint = 'MerchantAPI MapsServiceConfigUsageUpdateEndpoint';

-- {"api":"PostMerchantServiceUsageConfigMapsUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_MAPS_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantServiceUsageConfigMapsUpdateEndpoint';

-- {"api":"PostMerchantServiceConfigSmsUpdate","migration":"endpoint","param":"MerchantAPI SmsServiceConfigUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_SMS_UPDATE'
  WHERE endpoint = 'MerchantAPI SmsServiceConfigUpdateEndpoint';

-- {"api":"PostMerchantServiceConfigSmsUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_SMS_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantServiceConfigSmsUpdateEndpoint';

-- {"api":"PostMerchantServiceUsageConfigSmsUpdate","migration":"endpoint","param":"MerchantAPI SmsServiceConfigUsageUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_SMS_UPDATE'
  WHERE endpoint = 'MerchantAPI SmsServiceConfigUsageUpdateEndpoint';

-- {"api":"PostMerchantServiceUsageConfigSmsUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_SMS_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantServiceUsageConfigSmsUpdateEndpoint';

-- {"api":"PostMerchantServiceConfigVerificationUpdate","migration":"endpoint","param":"MerchantAPI VerificationServiceConfigUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_VERIFICATION_UPDATE'
  WHERE endpoint = 'MerchantAPI VerificationServiceConfigUpdateEndpoint';

-- {"api":"PostMerchantServiceConfigVerificationUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_VERIFICATION_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantServiceConfigVerificationUpdateEndpoint';

-- {"api":"PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreate","migration":"endpoint","param":"MerchantAPI CreateFPDriverExtraFeeEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_DRIVER_EXTRA_FEE_BOUNDS_CREATE'
  WHERE endpoint = 'MerchantAPI CreateFPDriverExtraFeeEndpoint';

-- {"api":"PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_DRIVER_EXTRA_FEE_BOUNDS_CREATE'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreateEndpoint';

-- {"api":"PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate","migration":"endpoint","param":"MerchantAPI UpdateFPDriverExtraFeeEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_DRIVER_EXTRA_FEE_BOUNDS_UPDATE'
  WHERE endpoint = 'MerchantAPI UpdateFPDriverExtraFeeEndpoint';

-- {"api":"PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_DRIVER_EXTRA_FEE_BOUNDS_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdateEndpoint';

-- {"api":"PostMerchantConfigFarePolicyPerExtraKmRateUpdate","migration":"endpoint","param":"MerchantAPI UpdateFPPerExtraKmRate","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_PER_EXTRA_KM_RATE_UPDATE'
  WHERE endpoint = 'MerchantAPI UpdateFPPerExtraKmRate';

-- {"api":"PostMerchantConfigFarePolicyPerExtraKmRateUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_PER_EXTRA_KM_RATE_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigFarePolicyPerExtraKmRateUpdateEndpoint';

-- {"api":"PostMerchantConfigFarePolicyUpdate","migration":"endpoint","param":"MerchantAPI UpdateFarePolicy","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_UPDATE'
  WHERE endpoint = 'MerchantAPI UpdateFarePolicy';

-- {"api":"PostMerchantConfigFarePolicyUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_UPDATE'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigFarePolicyUpdateEndpoint';

-- {"api":"PostMerchantConfigFarePolicyUpsert","migration":"endpoint","param":"MerchantAPI UpsertFarePolicyEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_UPSERT'
  WHERE endpoint = 'MerchantAPI UpsertFarePolicyEndpoint';

-- {"api":"PostMerchantConfigFarePolicyUpsert","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_UPSERT'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigFarePolicyUpsertEndpoint';

-- {"api":"PostMerchantConfigOperatingCityCreate","migration":"endpoint","param":"MerchantAPI CreateMerchantOperatingCityEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_CREATE'
  WHERE endpoint = 'MerchantAPI CreateMerchantOperatingCityEndpoint';

-- {"api":"PostMerchantConfigOperatingCityCreate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_CREATE'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigOperatingCityCreateEndpoint';

-- {"api":"PostMerchantUpdateOnboardingVehicleVariantMapping","migration":"endpoint","param":"MerchantAPI UpdateOnboardingVehicleVariantMappingEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE_ONBOARDING_VEHICLE_VARIANT_MAPPING'
  WHERE endpoint = 'MerchantAPI UpdateOnboardingVehicleVariantMappingEndpoint';

-- {"api":"PostMerchantUpdateOnboardingVehicleVariantMapping","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE_ONBOARDING_VEHICLE_VARIANT_MAPPING'
  WHERE endpoint = 'MerchantAPI PostMerchantUpdateOnboardingVehicleVariantMappingEndpoint';

-- {"api":"PostMerchantSpecialLocationUpsert","migration":"endpoint","param":"MerchantAPI UpsertSpecialLocationEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_UPSERT'
  WHERE endpoint = 'MerchantAPI UpsertSpecialLocationEndpoint';

-- {"api":"PostMerchantSpecialLocationUpsert","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_UPSERT'
  WHERE endpoint = 'MerchantAPI PostMerchantSpecialLocationUpsertEndpoint';

-- {"api":"DeleteMerchantSpecialLocationDelete","migration":"endpoint","param":"MerchantAPI DeleteSpecialLocationEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_DELETE'
  WHERE endpoint = 'MerchantAPI DeleteSpecialLocationEndpoint';

-- {"api":"DeleteMerchantSpecialLocationDelete","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_DELETE'
  WHERE endpoint = 'MerchantAPI DeleteMerchantSpecialLocationDeleteEndpoint';

-- {"api":"PostMerchantSpecialLocationGatesUpsert","migration":"endpoint","param":"MerchantAPI UpsertSpecialLocationGateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_GATES_UPSERT'
  WHERE endpoint = 'MerchantAPI UpsertSpecialLocationGateEndpoint';

-- {"api":"PostMerchantSpecialLocationGatesUpsert","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_GATES_UPSERT'
  WHERE endpoint = 'MerchantAPI PostMerchantSpecialLocationGatesUpsertEndpoint';

-- {"api":"DeleteMerchantSpecialLocationGatesDelete","migration":"endpoint","param":"MerchantAPI DeleteSpecialLocationGateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_GATES_DELETE'
  WHERE endpoint = 'MerchantAPI DeleteSpecialLocationGateEndpoint';

-- {"api":"DeleteMerchantSpecialLocationGatesDelete","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_GATES_DELETE'
  WHERE endpoint = 'MerchantAPI DeleteMerchantSpecialLocationGatesDeleteEndpoint';


------- SQL updates -------

-- {"api":"PostMerchantUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT MERCHANT_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'MERCHANT_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetMerchantConfigCommon","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT MERCHANT_COMMON_CONFIG","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_COMMON' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'MERCHANT_COMMON_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantConfigCommonUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT MERCHANT_COMMON_CONFIG_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_COMMON_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'MERCHANT_COMMON_CONFIG_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetMerchantConfigDriverPool","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT DRIVER_POOL_CONFIG","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_DRIVER_POOL' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'DRIVER_POOL_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantConfigDriverPoolUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT DRIVER_POOL_CONFIG_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_POOL_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'DRIVER_POOL_CONFIG_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantConfigDriverPoolCreate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT DRIVER_POOL_CONFIG_CREATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_POOL_CREATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'DRIVER_POOL_CONFIG_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetMerchantConfigDriverIntelligentPool","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT DRIVER_INTELLIGENT_POOL_CONFIG","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_DRIVER_INTELLIGENT_POOL' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'DRIVER_INTELLIGENT_POOL_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantConfigDriverIntelligentPoolUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT DRIVER_INTELLIGENT_POOL_CONFIG_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_INTELLIGENT_POOL_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'DRIVER_INTELLIGENT_POOL_CONFIG_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetMerchantConfigOnboardingDocument","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT ONBOARDING_DOCUMENT_CONFIG","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_ONBOARDING_DOCUMENT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'ONBOARDING_DOCUMENT_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantConfigOnboardingDocumentUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT ONBOARDING_DOCUMENT_CONFIG_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_ONBOARDING_DOCUMENT_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'ONBOARDING_DOCUMENT_CONFIG_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantConfigOnboardingDocumentCreate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT ONBOARDING_DOCUMENT_CONFIG_CREATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_ONBOARDING_DOCUMENT_CREATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'ONBOARDING_DOCUMENT_CONFIG_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetMerchantServiceUsageConfig","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT SERVICE_USAGE_CONFIG","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_SERVICE_USAGE_CONFIG' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'SERVICE_USAGE_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantServiceConfigMapsUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT MAPS_SERVICE_CONFIG_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_MAPS_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'MAPS_SERVICE_CONFIG_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantServiceUsageConfigMapsUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT MAPS_SERVICE_USAGE_CONFIG_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_MAPS_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'MAPS_SERVICE_USAGE_CONFIG_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantServiceConfigSmsUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT SMS_SERVICE_CONFIG_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_SMS_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'SMS_SERVICE_CONFIG_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantServiceUsageConfigSmsUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT SMS_SERVICE_USAGE_CONFIG_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_SMS_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'SMS_SERVICE_USAGE_CONFIG_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantServiceConfigVerificationUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT VERIFICATION_SERVICE_CONFIG_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_VERIFICATION_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'VERIFICATION_SERVICE_CONFIG_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT CREATE_FP_DRIVER_EXTRA_FEE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_DRIVER_EXTRA_FEE_BOUNDS_CREATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'CREATE_FP_DRIVER_EXTRA_FEE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT UPDATE_FP_DRIVER_EXTRA_FEE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_DRIVER_EXTRA_FEE_BOUNDS_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'UPDATE_FP_DRIVER_EXTRA_FEE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantConfigFarePolicyPerExtraKmRateUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT UPDATE_FP_PER_EXTRA_KM_RATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_PER_EXTRA_KM_RATE_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'UPDATE_FP_PER_EXTRA_KM_RATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantConfigFarePolicyUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT UPDATE_FARE_POLICY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'UPDATE_FARE_POLICY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantConfigFarePolicyUpsert","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT UPSERT_FARE_POLICY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_UPSERT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'UPSERT_FARE_POLICY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantConfigOperatingCityCreate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT CREATE_MERCHANT_OPERATING_CITY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_CREATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'CREATE_MERCHANT_OPERATING_CITY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantSchedulerTrigger","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT SCHEDULER_TRIGGER","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SCHEDULER_TRIGGER' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'SCHEDULER_TRIGGER' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantUpdateOnboardingVehicleVariantMapping","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT UPDATE_ONBOARDING_VEHICLE_VARIANT_MAPPING","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE_ONBOARDING_VEHICLE_VARIANT_MAPPING' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'UPDATE_ONBOARDING_VEHICLE_VARIANT_MAPPING' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantSpecialLocationUpsert","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT UPSERT_SPECIAL_LOCATION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_UPSERT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'UPSERT_SPECIAL_LOCATION' ) ON CONFLICT DO NOTHING;

-- {"api":"DeleteMerchantSpecialLocationDelete","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT DELETE_SPECIAL_LOCATION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_DELETE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'DELETE_SPECIAL_LOCATION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantSpecialLocationGatesUpsert","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT UPSERT_SPECIAL_LOCATION_GATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_GATES_UPSERT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'UPSERT_SPECIAL_LOCATION_GATE' ) ON CONFLICT DO NOTHING;

-- {"api":"DeleteMerchantSpecialLocationGatesDelete","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT DELETE_SPECIAL_LOCATION_GATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_GATES_DELETE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'DELETE_SPECIAL_LOCATION_GATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMerchantConfigClearCacheSubscription","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT CLEAR_CACHE_SUBSCRIPTION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_CLEAR_CACHE_SUBSCRIPTION' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'CLEAR_CACHE_SUBSCRIPTION' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostMerchantConfigFailover","migration":"endpoint","param":"MerchantAPI ToggleConfigPriorityEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FAILOVER'
  WHERE endpoint = 'MerchantAPI ToggleConfigPriorityEndpoint';

-- {"api":"PostMerchantConfigFailover","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FAILOVER'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigFailoverEndpoint';

-- {"api":"PostMerchantConfigFailover","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT TOGGLE_CONFIG_PRIORITY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FAILOVER' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'TOGGLE_CONFIG_PRIORITY' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostMerchantPayoutConfigUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT UPDATE_PAYOUT_CONFIG","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_PAYOUT_CONFIG_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'UPDATE_PAYOUT_CONFIG' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostMerchantConfigSpecialLocationUpsert","migration":"endpoint","param":"MerchantAPI UpsertSpecialLocationCsvEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_SPECIAL_LOCATION_UPSERT'
  WHERE endpoint = 'MerchantAPI UpsertSpecialLocationCsvEndpoint';

-- {"api":"PostMerchantConfigSpecialLocationUpsert","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_SPECIAL_LOCATION_UPSERT'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigSpecialLocationUpsertEndpoint';

-- {"api":"PostMerchantConfigSpecialLocationUpsert","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT UPSERT_SPECIAL_LOCATION_CSV","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_SPECIAL_LOCATION_UPSERT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'UPSERT_SPECIAL_LOCATION_CSV' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostMerchantConfigUpsertPlanAndConfigSubscription","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MERCHANT UPSERT_PLAN_AND_SUBSCRIPTION_CONFIG","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_UPSERT_PLAN_AND_CONFIG_SUBSCRIPTION' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'UPSERT_PLAN_AND_SUBSCRIPTION_CONFIG' ) ON CONFLICT DO NOTHING;
