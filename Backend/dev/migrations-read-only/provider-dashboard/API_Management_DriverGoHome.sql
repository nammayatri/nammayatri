-- {"api":"PostDriverGoHomeUpdateHomeLocation","migration":"endpoint","param":"DriverAPI UpdateDriverHomeLocationEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/POST_DRIVER_GO_HOME_UPDATE_HOME_LOCATION'
  WHERE endpoint = 'DriverAPI UpdateDriverHomeLocationEndpoint';

-- {"api":"PostDriverGoHomeUpdateHomeLocation","migration":"endpointV2","param":null,"schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/POST_DRIVER_GO_HOME_UPDATE_HOME_LOCATION'
  WHERE endpoint = 'DriverGoHomeAPI PostDriverGoHomeUpdateHomeLocationEndpoint';

-- {"api":"PostDriverGoHomeIncrementGoToCount","migration":"endpoint","param":"DriverAPI IncrementDriverGoToCountEndPoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/POST_DRIVER_GO_HOME_INCREMENT_GO_TO_COUNT'
  WHERE endpoint = 'DriverAPI IncrementDriverGoToCountEndPoint';

-- {"api":"PostDriverGoHomeIncrementGoToCount","migration":"endpointV2","param":null,"schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/POST_DRIVER_GO_HOME_INCREMENT_GO_TO_COUNT'
  WHERE endpoint = 'DriverGoHomeAPI PostDriverGoHomeIncrementGoToCountEndpoint';

------- SQL updates -------
