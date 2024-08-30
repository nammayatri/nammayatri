-- {"api":"PostNammaTagTagCreate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_CREATE'
  WHERE endpoint = 'NammaTagAPI PostNammaTagTagCreateEndpoint';

-- {"api":"PostNammaTagQueryCreate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_QUERY_CREATE'
  WHERE endpoint = 'NammaTagAPI PostNammaTagQueryCreateEndpoint';

-- {"api":"PostNammaTagAppDynamicLogicVerify","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERIFY'
  WHERE endpoint = 'NammaTagAPI PostNammaTagAppDynamicLogicVerifyEndpoint';
