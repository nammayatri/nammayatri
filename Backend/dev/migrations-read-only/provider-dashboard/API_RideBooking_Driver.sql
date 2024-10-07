-- {"api":"PostDriverEnable","migration":"endpoint","param":"DriverAPI EnableDriverEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_ENABLE'
  WHERE endpoint = 'DriverAPI EnableDriverEndpoint';

-- {"api":"PostDriverEnable","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_ENABLE'
  WHERE endpoint = 'DriverAPI PostDriverEnableEndpoint';

-- {"api":"PostDriverCollectCash","migration":"endpoint","param":"DriverAPI CollectCashEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_COLLECT_CASH'
  WHERE endpoint = 'DriverAPI CollectCashEndpoint';

-- {"api":"PostDriverCollectCash","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_COLLECT_CASH'
  WHERE endpoint = 'DriverAPI PostDriverCollectCashEndpoint';

-- {"api":"PostDriverV2CollectCash","migration":"endpoint","param":"DriverAPI CollectCashEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_V2_COLLECT_CASH'
  WHERE endpoint = 'DriverAPI CollectCashEndpoint';

-- {"api":"PostDriverV2CollectCash","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_V2_COLLECT_CASH'
  WHERE endpoint = 'DriverAPI PostDriverV2CollectCashEndpoint';

-- {"api":"PostDriverExemptCash","migration":"endpoint","param":"DriverAPI ExemptCashEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_EXEMPT_CASH'
  WHERE endpoint = 'DriverAPI ExemptCashEndpoint';

-- {"api":"PostDriverExemptCash","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_EXEMPT_CASH'
  WHERE endpoint = 'DriverAPI PostDriverExemptCashEndpoint';

-- {"api":"PostDriverV2ExemptCash","migration":"endpoint","param":"DriverAPI ExemptCashEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_V2_EXEMPT_CASH'
  WHERE endpoint = 'DriverAPI ExemptCashEndpoint';

-- {"api":"PostDriverV2ExemptCash","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_V2_EXEMPT_CASH'
  WHERE endpoint = 'DriverAPI PostDriverV2ExemptCashEndpoint';

-- {"api":"PostDriverUnlinkVehicle","migration":"endpoint","param":"DriverAPI UnlinkVehicleEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_UNLINK_VEHICLE'
  WHERE endpoint = 'DriverAPI UnlinkVehicleEndpoint';

-- {"api":"PostDriverUnlinkVehicle","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_UNLINK_VEHICLE'
  WHERE endpoint = 'DriverAPI PostDriverUnlinkVehicleEndpoint';

-- {"api":"PostDriverEndRCAssociation","migration":"endpoint","param":"DriverAPI EndRCAssociationEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_END_RC_ASSOCIATION'
  WHERE endpoint = 'DriverAPI EndRCAssociationEndpoint';

-- {"api":"PostDriverEndRCAssociation","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_END_RC_ASSOCIATION'
  WHERE endpoint = 'DriverAPI PostDriverEndRCAssociationEndpoint';

-- {"api":"PostDriverAddVehicle","migration":"endpoint","param":"DriverAPI AddVehicleEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_ADD_VEHICLE'
  WHERE endpoint = 'DriverAPI AddVehicleEndpoint';

-- {"api":"PostDriverAddVehicle","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_ADD_VEHICLE'
  WHERE endpoint = 'DriverAPI PostDriverAddVehicleEndpoint';

-- {"api":"PostDriverSetRCStatus","migration":"endpoint","param":"DriverAPI SetRCStatusEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_SET_RC_STATUS'
  WHERE endpoint = 'DriverAPI SetRCStatusEndpoint';

-- {"api":"PostDriverSetRCStatus","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_SET_RC_STATUS'
  WHERE endpoint = 'DriverAPI PostDriverSetRCStatusEndpoint';


------- SQL updates -------

-- {"api":"PostDriverExemptDriverFee","migration":"endpoint","param":"DriverAPI ExemptionAndCashCollectionDriverFeeEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_EXEMPT_DRIVER_FEE'
  WHERE endpoint = 'DriverAPI ExemptionAndCashCollectionDriverFeeEndpoint';
