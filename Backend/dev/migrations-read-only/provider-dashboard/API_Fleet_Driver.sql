-- {"api":"PostDriverFleetVehicleDriverRCstatus","migration":"endpoint","param":"DriverAPI SetVehicleDriverRcStatusForFleetEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderFleetAPI_DriverAPI_PostDriverFleetVehicleDriverRCstatusEndpoint'
  WHERE endpoint = 'DriverAPI SetVehicleDriverRcStatusForFleetEndpoint';

-- {"api":"PostDriverFleetVehicleDriverRCstatus","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderFleetAPI_DriverAPI_PostDriverFleetVehicleDriverRCstatusEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverFleetVehicleDriverRCstatusEndpoint';

-- {"api":"PostDriverUpdateFleetOwnerInfo","migration":"endpoint","param":"DriverAPI UpdateFleetOwnerEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderFleetAPI_DriverAPI_PostDriverUpdateFleetOwnerInfoEndpoint'
  WHERE endpoint = 'DriverAPI UpdateFleetOwnerEndPoint';

-- {"api":"PostDriverUpdateFleetOwnerInfo","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderFleetAPI_DriverAPI_PostDriverUpdateFleetOwnerInfoEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverUpdateFleetOwnerInfoEndpoint';
