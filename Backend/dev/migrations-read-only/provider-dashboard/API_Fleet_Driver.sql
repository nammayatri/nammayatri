-- {"api":"PostDriverFleetVehicleDriverRcStatus","migration":"endpoint","param":"DriverAPI SetVehicleDriverRcStatusForFleetEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VEHICLE_DRIVER_RC_STATUS'
  WHERE endpoint = 'DriverAPI SetVehicleDriverRcStatusForFleetEndpoint';

-- {"api":"PostDriverFleetVehicleDriverRcStatus","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VEHICLE_DRIVER_RC_STATUS'
  WHERE endpoint = 'DriverAPI PostDriverFleetVehicleDriverRCstatusEndpoint';

-- {"api":"PostDriverUpdateFleetOwnerInfo","migration":"endpoint","param":"DriverAPI UpdateFleetOwnerEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_FLEET/DRIVER/POST_DRIVER_UPDATE_FLEET_OWNER_INFO'
  WHERE endpoint = 'DriverAPI UpdateFleetOwnerEndPoint';

-- {"api":"PostDriverUpdateFleetOwnerInfo","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_FLEET/DRIVER/POST_DRIVER_UPDATE_FLEET_OWNER_INFO'
  WHERE endpoint = 'DriverAPI PostDriverUpdateFleetOwnerInfoEndpoint';
