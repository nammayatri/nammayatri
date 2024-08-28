-- {"api":"PostNammaTagTagCreate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_NammaTagAPI_PostNammaTagTagCreateEndpoint'
  WHERE endpoint = 'NammaTagAPI PostNammaTagTagCreateEndpoint';

-- {"api":"PostNammaTagQueryCreate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_NammaTagAPI_PostNammaTagQueryCreateEndpoint'
  WHERE endpoint = 'NammaTagAPI PostNammaTagQueryCreateEndpoint';

-- {"api":"PostNammaTagAppDynamicLogicVerify","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_NammaTagAPI_PostNammaTagAppDynamicLogicVerifyEndpoint'
  WHERE endpoint = 'NammaTagAPI PostNammaTagAppDynamicLogicVerifyEndpoint';
