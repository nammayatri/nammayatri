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
