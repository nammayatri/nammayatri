-- {"api":"PostDriverPersonNumbers","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverPersonNumbersEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverPersonNumbersEndpoint';

-- {"api":"PostDriverDisable","migration":"endpoint","param":"DriverAPI DisableDriverEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverDisableEndpoint'
  WHERE endpoint = 'DriverAPI DisableDriverEndpoint';

-- {"api":"PostDriverDisable","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverDisableEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverDisableEndpoint';

-- {"api":"PostDriverAcRestrictionUpdate","migration":"endpoint","param":"DriverAPI RemoveACUsageRestrictionEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverAcRestrictionUpdateEndpoint'
  WHERE endpoint = 'DriverAPI RemoveACUsageRestrictionEndpoint';

-- {"api":"PostDriverAcRestrictionUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverAcRestrictionUpdateEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverAcRestrictionUpdateEndpoint';

-- {"api":"PostDriverBlockWithReason","migration":"endpoint","param":"DriverAPI BlockDriverWithReasonEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverBlockWithReasonEndpoint'
  WHERE endpoint = 'DriverAPI BlockDriverWithReasonEndpoint';

-- {"api":"PostDriverBlockWithReason","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverBlockWithReasonEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverBlockWithReasonEndpoint';

-- {"api":"PostDriverBlock","migration":"endpoint","param":"DriverAPI BlockDriverEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverBlockEndpoint'
  WHERE endpoint = 'DriverAPI BlockDriverEndpoint';

-- {"api":"PostDriverBlock","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverBlockEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverBlockEndpoint';

-- {"api":"PostDriverUnblock","migration":"endpoint","param":"DriverAPI UnblockDriverEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUnblockEndpoint'
  WHERE endpoint = 'DriverAPI UnblockDriverEndpoint';

-- {"api":"PostDriverUnblock","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUnblockEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverUnblockEndpoint';

-- {"api":"DeleteDriverPermanentlyDelete","migration":"endpoint","param":"DriverAPI DeleteDriverEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_DeleteDriverPermanentlyDeleteEndpoint'
  WHERE endpoint = 'DriverAPI DeleteDriverEndpoint';

-- {"api":"DeleteDriverPermanentlyDelete","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_DeleteDriverPermanentlyDeleteEndpoint'
  WHERE endpoint = 'DriverAPI DeleteDriverPermanentlyDeleteEndpoint';

-- {"api":"PostDriverUnlinkDL","migration":"endpoint","param":"DriverAPI UnlinkDLEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUnlinkDLEndpoint'
  WHERE endpoint = 'DriverAPI UnlinkDLEndpoint';

-- {"api":"PostDriverUnlinkDL","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUnlinkDLEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverUnlinkDLEndpoint';

-- {"api":"PostDriverUnlinkAadhaar","migration":"endpoint","param":"DriverAPI UnlinkAadhaarEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUnlinkAadhaarEndpoint'
  WHERE endpoint = 'DriverAPI UnlinkAadhaarEndpoint';

-- {"api":"PostDriverUnlinkAadhaar","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUnlinkAadhaarEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverUnlinkAadhaarEndpoint';

-- {"api":"PostDriverUpdatePhoneNumber","migration":"endpoint","param":"DriverAPI UpdatePhoneNumberEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUpdatePhoneNumberEndpoint'
  WHERE endpoint = 'DriverAPI UpdatePhoneNumberEndpoint';

-- {"api":"PostDriverUpdatePhoneNumber","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUpdatePhoneNumberEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverUpdatePhoneNumberEndpoint';

-- {"api":"PostDriverUpdateName","migration":"endpoint","param":"DriverAPI UpdateDriverNameEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUpdateNameEndpoint'
  WHERE endpoint = 'DriverAPI UpdateDriverNameEndpoint';

-- {"api":"PostDriverUpdateName","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUpdateNameEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverUpdateNameEndpoint';

-- {"api":"PostDriverDeleteRC","migration":"endpoint","param":"DriverAPI DeleteRCEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverDeleteRCEndpoint'
  WHERE endpoint = 'DriverAPI DeleteRCEndpoint';

-- {"api":"PostDriverDeleteRC","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverDeleteRCEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverDeleteRCEndpoint';

-- {"api":"PostDriverSendDummyNotification","migration":"endpoint","param":"DriverAPI SendDummyRideRequestToDriverViaDashboardEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverSendDummyNotificationEndpoint'
  WHERE endpoint = 'DriverAPI SendDummyRideRequestToDriverViaDashboardEndPoint';

-- {"api":"PostDriverSendDummyNotification","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverSendDummyNotificationEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverSendDummyNotificationEndpoint';

-- {"api":"PostDriverChangeOperatingCity","migration":"endpoint","param":"DriverAPI ChangeOperatingCityEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverChangeOperatingCityEndpoint'
  WHERE endpoint = 'DriverAPI ChangeOperatingCityEndpoint';

-- {"api":"PostDriverChangeOperatingCity","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverChangeOperatingCityEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverChangeOperatingCityEndpoint';

-- {"api":"PostDriverPauseOrResumeServiceCharges","migration":"endpoint","param":"DriverAPI PauseOrResumeServiceChargesEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverPauseOrResumeServiceChargesEndpoint'
  WHERE endpoint = 'DriverAPI PauseOrResumeServiceChargesEndPoint';

-- {"api":"PostDriverPauseOrResumeServiceCharges","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverPauseOrResumeServiceChargesEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverPauseOrResumeServiceChargesEndpoint';

-- {"api":"PostDriverUpdateRCInvalidStatus","migration":"endpoint","param":"DriverAPI UpdateRCInvalidStatusEndPoint65454","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUpdateRCInvalidStatusEndpoint'
  WHERE endpoint = 'DriverAPI UpdateRCInvalidStatusEndPoint65454';

-- {"api":"PostDriverUpdateRCInvalidStatus","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUpdateRCInvalidStatusEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverUpdateRCInvalidStatusEndpoint';

-- {"api":"PostDriverUpdateVehicleVariant","migration":"endpoint","param":"DriverAPI UpdateVehicleVariantEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUpdateVehicleVariantEndpoint'
  WHERE endpoint = 'DriverAPI UpdateVehicleVariantEndPoint';

-- {"api":"PostDriverUpdateVehicleVariant","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUpdateVehicleVariantEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverUpdateVehicleVariantEndpoint';

-- {"api":"PostDriverBulkReviewRCVariant","migration":"endpoint","param":"DriverAPI BulkReviewRCVariantEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverBulkReviewRCVariantEndpoint'
  WHERE endpoint = 'DriverAPI BulkReviewRCVariantEndPoint';

-- {"api":"PostDriverBulkReviewRCVariant","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverBulkReviewRCVariantEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverBulkReviewRCVariantEndpoint';

-- {"api":"PostDriverUpdateDriverTag","migration":"endpoint","param":"DriverAPI UpdateDriverTagEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUpdateDriverTagEndpoint'
  WHERE endpoint = 'DriverAPI UpdateDriverTagEndPoint';

-- {"api":"PostDriverUpdateDriverTag","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverUpdateDriverTagEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverUpdateDriverTagEndpoint';

-- {"api":"PostDriverClearFee","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverClearFeeEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverClearFeeEndpoint';

-- {"api":"PostDriverSyncDocAadharPan","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_DriverAPI_PostDriverSyncDocAadharPanEndpoint'
  WHERE endpoint = 'DriverAPI PostDriverSyncDocAadharPanEndpoint';
