-- {"api":"PostDriverFleetVehicleDriverRcStatus","migration":"endpoint","param":"DriverAPI SetVehicleDriverRcStatusForFleetEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VEHICLE_DRIVER_RC_STATUS'
  WHERE endpoint = 'DriverAPI SetVehicleDriverRcStatusForFleetEndpoint';

-- {"api":"PostDriverFleetVehicleDriverRcStatus","migration":"endpointV2","param":null,"schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VEHICLE_DRIVER_RC_STATUS'
  WHERE endpoint = 'DriverAPI PostDriverFleetVehicleDriverRCstatusEndpoint';

-- {"api":"PostDriverUpdateFleetOwnerInfo","migration":"endpoint","param":"DriverAPI UpdateFleetOwnerEndPoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_FLEET/DRIVER/POST_DRIVER_UPDATE_FLEET_OWNER_INFO'
  WHERE endpoint = 'DriverAPI UpdateFleetOwnerEndPoint';

-- {"api":"PostDriverUpdateFleetOwnerInfo","migration":"endpointV2","param":null,"schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_FLEET/DRIVER/POST_DRIVER_UPDATE_FLEET_OWNER_INFO'
  WHERE endpoint = 'DriverAPI PostDriverUpdateFleetOwnerInfoEndpoint';

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

-- {"api":"PostDriverFleetVehicleChangeFleetOwner","migration":"capability","param":"city-operations.onboarding.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VEHICLE_CHANGE_FLEET_OWNER' ) ON CONFLICT DO NOTHING;
-- {"api":"PostDriverFleetCashRideUpdate","migration":"capability","param":"city-operations.driver_management.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.driver_management.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_CASH_RIDE_UPDATE' ) ON CONFLICT DO NOTHING;
