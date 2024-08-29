-- {"api":"PostMerchantUpdate","migration":"endpoint","param":"MerchantAPI MerchantUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI MerchantUpdateEndpoint';

-- {"api":"PostMerchantUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantUpdateEndpoint';

-- {"api":"PostMerchantConfigCommonUpdate","migration":"endpoint","param":"MerchantAPI MerchantCommonConfigUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigCommonUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI MerchantCommonConfigUpdateEndpoint';

-- {"api":"PostMerchantConfigCommonUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigCommonUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigCommonUpdateEndpoint';

-- {"api":"PostMerchantConfigDriverPoolUpdate","migration":"endpoint","param":"MerchantAPI DriverPoolConfigUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigDriverPoolUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI DriverPoolConfigUpdateEndpoint';

-- {"api":"PostMerchantConfigDriverPoolUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigDriverPoolUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigDriverPoolUpdateEndpoint';

-- {"api":"PostMerchantConfigDriverPoolCreate","migration":"endpoint","param":"MerchantAPI DriverPoolConfigCreateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigDriverPoolCreateEndpoint'
  WHERE endpoint = 'MerchantAPI DriverPoolConfigCreateEndpoint';

-- {"api":"PostMerchantConfigDriverPoolCreate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigDriverPoolCreateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigDriverPoolCreateEndpoint';

-- {"api":"PostMerchantConfigDriverIntelligentPoolUpdate","migration":"endpoint","param":"MerchantAPI DriverIntelligentPoolConfigUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigDriverIntelligentPoolUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI DriverIntelligentPoolConfigUpdateEndpoint';

-- {"api":"PostMerchantConfigDriverIntelligentPoolUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigDriverIntelligentPoolUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigDriverIntelligentPoolUpdateEndpoint';

-- {"api":"PostMerchantConfigOnboardingDocumentUpdate","migration":"endpoint","param":"MerchantAPI DocumentVerificationConfigUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigOnboardingDocumentUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI DocumentVerificationConfigUpdateEndpoint';

-- {"api":"PostMerchantConfigOnboardingDocumentUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigOnboardingDocumentUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigOnboardingDocumentUpdateEndpoint';

-- {"api":"PostMerchantConfigOnboardingDocumentCreate","migration":"endpoint","param":"MerchantAPI DocumentVerificationConfigCreateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigOnboardingDocumentCreateEndpoint'
  WHERE endpoint = 'MerchantAPI DocumentVerificationConfigCreateEndpoint';

-- {"api":"PostMerchantConfigOnboardingDocumentCreate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigOnboardingDocumentCreateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigOnboardingDocumentCreateEndpoint';

-- {"api":"PostMerchantServiceConfigMapsUpdate","migration":"endpoint","param":"MerchantAPI MapsServiceConfigUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantServiceConfigMapsUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI MapsServiceConfigUpdateEndpoint';

-- {"api":"PostMerchantServiceConfigMapsUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantServiceConfigMapsUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantServiceConfigMapsUpdateEndpoint';

-- {"api":"PostMerchantServiceUsageConfigMapsUpdate","migration":"endpoint","param":"MerchantAPI MapsServiceConfigUsageUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantServiceUsageConfigMapsUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI MapsServiceConfigUsageUpdateEndpoint';

-- {"api":"PostMerchantServiceUsageConfigMapsUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantServiceUsageConfigMapsUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantServiceUsageConfigMapsUpdateEndpoint';

-- {"api":"PostMerchantServiceConfigSmsUpdate","migration":"endpoint","param":"MerchantAPI SmsServiceConfigUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantServiceConfigSmsUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI SmsServiceConfigUpdateEndpoint';

-- {"api":"PostMerchantServiceConfigSmsUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantServiceConfigSmsUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantServiceConfigSmsUpdateEndpoint';

-- {"api":"PostMerchantServiceUsageConfigSmsUpdate","migration":"endpoint","param":"MerchantAPI SmsServiceConfigUsageUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantServiceUsageConfigSmsUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI SmsServiceConfigUsageUpdateEndpoint';

-- {"api":"PostMerchantServiceUsageConfigSmsUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantServiceUsageConfigSmsUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantServiceUsageConfigSmsUpdateEndpoint';

-- {"api":"PostMerchantServiceConfigVerificationUpdate","migration":"endpoint","param":"MerchantAPI VerificationServiceConfigUpdateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantServiceConfigVerificationUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI VerificationServiceConfigUpdateEndpoint';

-- {"api":"PostMerchantServiceConfigVerificationUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantServiceConfigVerificationUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantServiceConfigVerificationUpdateEndpoint';

-- {"api":"PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreate","migration":"endpoint","param":"MerchantAPI CreateFPDriverExtraFeeEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreateEndpoint'
  WHERE endpoint = 'MerchantAPI CreateFPDriverExtraFeeEndpoint';

-- {"api":"PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreateEndpoint';

-- {"api":"PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate","migration":"endpoint","param":"MerchantAPI UpdateFPDriverExtraFeeEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI UpdateFPDriverExtraFeeEndpoint';

-- {"api":"PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdateEndpoint';

-- {"api":"PostMerchantConfigFarePolicyPerExtraKmRateUpdate","migration":"endpoint","param":"MerchantAPI UpdateFPPerExtraKmRate","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigFarePolicyPerExtraKmRateUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI UpdateFPPerExtraKmRate';

-- {"api":"PostMerchantConfigFarePolicyPerExtraKmRateUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigFarePolicyPerExtraKmRateUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigFarePolicyPerExtraKmRateUpdateEndpoint';

-- {"api":"PostMerchantConfigFarePolicyUpdate","migration":"endpoint","param":"MerchantAPI UpdateFarePolicy","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigFarePolicyUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI UpdateFarePolicy';

-- {"api":"PostMerchantConfigFarePolicyUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigFarePolicyUpdateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigFarePolicyUpdateEndpoint';

-- {"api":"PostMerchantConfigFarePolicyUpsert","migration":"endpoint","param":"MerchantAPI UpsertFarePolicyEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigFarePolicyUpsertEndpoint'
  WHERE endpoint = 'MerchantAPI UpsertFarePolicyEndpoint';

-- {"api":"PostMerchantConfigFarePolicyUpsert","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigFarePolicyUpsertEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigFarePolicyUpsertEndpoint';

-- {"api":"PostMerchantConfigOperatingCityCreate","migration":"endpoint","param":"MerchantAPI CreateMerchantOperatingCityEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigOperatingCityCreateEndpoint'
  WHERE endpoint = 'MerchantAPI CreateMerchantOperatingCityEndpoint';

-- {"api":"PostMerchantConfigOperatingCityCreate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantConfigOperatingCityCreateEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantConfigOperatingCityCreateEndpoint';

-- {"api":"PostMerchantUpdateOnboardingVehicleVariantMapping","migration":"endpoint","param":"MerchantAPI UpdateOnboardingVehicleVariantMappingEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantUpdateOnboardingVehicleVariantMappingEndpoint'
  WHERE endpoint = 'MerchantAPI UpdateOnboardingVehicleVariantMappingEndpoint';

-- {"api":"PostMerchantUpdateOnboardingVehicleVariantMapping","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantUpdateOnboardingVehicleVariantMappingEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantUpdateOnboardingVehicleVariantMappingEndpoint';

-- {"api":"PostMerchantSpecialLocationUpsert","migration":"endpoint","param":"MerchantAPI UpsertSpecialLocationEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantSpecialLocationUpsertEndpoint'
  WHERE endpoint = 'MerchantAPI UpsertSpecialLocationEndpoint';

-- {"api":"PostMerchantSpecialLocationUpsert","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantSpecialLocationUpsertEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantSpecialLocationUpsertEndpoint';

-- {"api":"DeleteMerchantSpecialLocationDelete","migration":"endpoint","param":"MerchantAPI DeleteSpecialLocationEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_DeleteMerchantSpecialLocationDeleteEndpoint'
  WHERE endpoint = 'MerchantAPI DeleteSpecialLocationEndpoint';

-- {"api":"DeleteMerchantSpecialLocationDelete","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_DeleteMerchantSpecialLocationDeleteEndpoint'
  WHERE endpoint = 'MerchantAPI DeleteMerchantSpecialLocationDeleteEndpoint';

-- {"api":"PostMerchantSpecialLocationGatesUpsert","migration":"endpoint","param":"MerchantAPI UpsertSpecialLocationGateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantSpecialLocationGatesUpsertEndpoint'
  WHERE endpoint = 'MerchantAPI UpsertSpecialLocationGateEndpoint';

-- {"api":"PostMerchantSpecialLocationGatesUpsert","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_PostMerchantSpecialLocationGatesUpsertEndpoint'
  WHERE endpoint = 'MerchantAPI PostMerchantSpecialLocationGatesUpsertEndpoint';

-- {"api":"DeleteMerchantSpecialLocationGatesDelete","migration":"endpoint","param":"MerchantAPI DeleteSpecialLocationGateEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_DeleteMerchantSpecialLocationGatesDeleteEndpoint'
  WHERE endpoint = 'MerchantAPI DeleteSpecialLocationGateEndpoint';

-- {"api":"DeleteMerchantSpecialLocationGatesDelete","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MerchantAPI_DeleteMerchantSpecialLocationGatesDeleteEndpoint'
  WHERE endpoint = 'MerchantAPI DeleteMerchantSpecialLocationGatesDeleteEndpoint';
