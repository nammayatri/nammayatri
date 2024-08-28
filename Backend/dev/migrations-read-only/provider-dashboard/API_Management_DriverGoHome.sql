-- {"api":"PostDriverGoHomeUpdateHomeLocation","migration":"endpoint","param":"DriverAPI UpdateDriverHomeLocationEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverGoHomeAPI_PostDriverGoHomeUpdateHomeLocationEndpoint'
  WHERE endpoint = 'DriverAPI UpdateDriverHomeLocationEndpoint';

-- {"api":"PostDriverGoHomeUpdateHomeLocation","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverGoHomeAPI_PostDriverGoHomeUpdateHomeLocationEndpoint'
  WHERE endpoint = 'DriverGoHomeAPI PostDriverGoHomeUpdateHomeLocationEndpoint';

-- {"api":"PostDriverGoHomeIncrementGoToCount","migration":"endpoint","param":"DriverAPI IncrementDriverGoToCountEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverGoHomeAPI_PostDriverGoHomeIncrementGoToCountEndpoint'
  WHERE endpoint = 'DriverAPI IncrementDriverGoToCountEndPoint';

-- {"api":"PostDriverGoHomeIncrementGoToCount","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverGoHomeAPI_PostDriverGoHomeIncrementGoToCountEndpoint'
  WHERE endpoint = 'DriverGoHomeAPI PostDriverGoHomeIncrementGoToCountEndpoint';
