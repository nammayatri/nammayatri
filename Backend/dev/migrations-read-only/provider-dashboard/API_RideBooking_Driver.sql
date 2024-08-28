-- {"api":"PostDriverEnable","migration":"endpoint","param":"DriverAPI EnableDriverEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverEnableEndpoint'
  WHERE endpoint = 'DriverAPI EnableDriverEndpoint';

-- {"api":"PostDriverEnable","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverEnableEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverEnableEndpoint';

-- {"api":"PostDriverCollectCash","migration":"endpoint","param":"DriverAPI CollectCashEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverCollectCashEndpoint'
  WHERE endpoint = 'DriverAPI CollectCashEndpoint';

-- {"api":"PostDriverCollectCash","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverCollectCashEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverCollectCashEndpoint';

-- {"api":"PostDriverV2CollectCash","migration":"endpoint","param":"DriverAPI CollectCashEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverV2CollectCashEndpoint'
  WHERE endpoint = 'DriverAPI CollectCashEndpoint';

-- {"api":"PostDriverV2CollectCash","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverV2CollectCashEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverV2CollectCashEndpoint';

-- {"api":"PostDriverExemptCash","migration":"endpoint","param":"DriverAPI ExemptCashEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverExemptCashEndpoint'
  WHERE endpoint = 'DriverAPI ExemptCashEndpoint';

-- {"api":"PostDriverExemptCash","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverExemptCashEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverExemptCashEndpoint';

-- {"api":"PostDriverV2ExemptCash","migration":"endpoint","param":"DriverAPI ExemptCashEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverV2ExemptCashEndpoint'
  WHERE endpoint = 'DriverAPI ExemptCashEndpoint';

-- {"api":"PostDriverV2ExemptCash","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverV2ExemptCashEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverV2ExemptCashEndpoint';

-- {"api":"PostDriverUnlinkVehicle","migration":"endpoint","param":"DriverAPI UnlinkVehicleEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverUnlinkVehicleEndpoint'
  WHERE endpoint = 'DriverAPI UnlinkVehicleEndpoint';

-- {"api":"PostDriverUnlinkVehicle","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverUnlinkVehicleEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverUnlinkVehicleEndpoint';

-- {"api":"PostDriverEndRCAssociation","migration":"endpoint","param":"DriverAPI EndRCAssociationEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverEndRCAssociationEndpoint'
  WHERE endpoint = 'DriverAPI EndRCAssociationEndpoint';

-- {"api":"PostDriverEndRCAssociation","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverEndRCAssociationEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverEndRCAssociationEndpoint';

-- {"api":"PostDriverAddVehicle","migration":"endpoint","param":"DriverAPI AddVehicleEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverAddVehicleEndpoint'
  WHERE endpoint = 'DriverAPI AddVehicleEndpoint';

-- {"api":"PostDriverAddVehicle","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverAddVehicleEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverAddVehicleEndpoint';

-- {"api":"PostDriverSetRCStatus","migration":"endpoint","param":"DriverAPI SetRCStatusEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverSetRCStatusEndpoint'
  WHERE endpoint = 'DriverAPI SetRCStatusEndpoint';

-- {"api":"PostDriverSetRCStatus","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderRideBookingAPI_DriverAPI_PostDriverSetRCStatusEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverSetRCStatusEndpoint';
