-- {"api":"PostDriverPersonNumbers","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_PERSON_NUMBERS'
  WHERE endpoint = 'DriverAPI PostDriverPersonNumbersEndpoint';

-- {"api":"PostDriverPersonId","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_PERSON_ID'
  WHERE endpoint = 'DriverAPI PostDriverPersonIdEndpoint';

-- {"api":"PostDriverDisable","migration":"endpoint","param":"DriverAPI DisableDriverEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_DISABLE'
  WHERE endpoint = 'DriverAPI DisableDriverEndpoint';

-- {"api":"PostDriverDisable","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_DISABLE'
  WHERE endpoint = 'DriverAPI PostDriverDisableEndpoint';

-- {"api":"PostDriverAcRestrictionUpdate","migration":"endpoint","param":"DriverAPI RemoveACUsageRestrictionEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_AC_RESTRICTION_UPDATE'
  WHERE endpoint = 'DriverAPI RemoveACUsageRestrictionEndpoint';

-- {"api":"PostDriverAcRestrictionUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_AC_RESTRICTION_UPDATE'
  WHERE endpoint = 'DriverAPI PostDriverAcRestrictionUpdateEndpoint';

-- {"api":"PostDriverBlockWithReason","migration":"endpoint","param":"DriverAPI BlockDriverWithReasonEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BLOCK_WITH_REASON'
  WHERE endpoint = 'DriverAPI BlockDriverWithReasonEndpoint';

-- {"api":"PostDriverBlockWithReason","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BLOCK_WITH_REASON'
  WHERE endpoint = 'DriverAPI PostDriverBlockWithReasonEndpoint';

-- {"api":"PostDriverBlock","migration":"endpoint","param":"DriverAPI BlockDriverEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BLOCK'
  WHERE endpoint = 'DriverAPI BlockDriverEndpoint';

-- {"api":"PostDriverBlock","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BLOCK'
  WHERE endpoint = 'DriverAPI PostDriverBlockEndpoint';

-- {"api":"PostDriverUnblock","migration":"endpoint","param":"DriverAPI UnblockDriverEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNBLOCK'
  WHERE endpoint = 'DriverAPI UnblockDriverEndpoint';

-- {"api":"PostDriverUnblock","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNBLOCK'
  WHERE endpoint = 'DriverAPI PostDriverUnblockEndpoint';

-- {"api":"DeleteDriverPermanentlyDelete","migration":"endpoint","param":"DriverAPI DeleteDriverEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/DELETE_DRIVER_PERMANENTLY_DELETE'
  WHERE endpoint = 'DriverAPI DeleteDriverEndpoint';

-- {"api":"DeleteDriverPermanentlyDelete","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/DELETE_DRIVER_PERMANENTLY_DELETE'
  WHERE endpoint = 'DriverAPI DeleteDriverPermanentlyDeleteEndpoint';

-- {"api":"PostDriverUnlinkDL","migration":"endpoint","param":"DriverAPI UnlinkDLEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNLINK_DL'
  WHERE endpoint = 'DriverAPI UnlinkDLEndpoint';

-- {"api":"PostDriverUnlinkDL","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNLINK_DL'
  WHERE endpoint = 'DriverAPI PostDriverUnlinkDLEndpoint';

-- {"api":"PostDriverUnlinkAadhaar","migration":"endpoint","param":"DriverAPI UnlinkAadhaarEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNLINK_AADHAAR'
  WHERE endpoint = 'DriverAPI UnlinkAadhaarEndpoint';

-- {"api":"PostDriverUnlinkAadhaar","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNLINK_AADHAAR'
  WHERE endpoint = 'DriverAPI PostDriverUnlinkAadhaarEndpoint';

-- {"api":"PostDriverUpdatePhoneNumber","migration":"endpoint","param":"DriverAPI UpdatePhoneNumberEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_PHONE_NUMBER'
  WHERE endpoint = 'DriverAPI UpdatePhoneNumberEndpoint';

-- {"api":"PostDriverUpdatePhoneNumber","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_PHONE_NUMBER'
  WHERE endpoint = 'DriverAPI PostDriverUpdatePhoneNumberEndpoint';

-- {"api":"PostDriverUpdateName","migration":"endpoint","param":"DriverAPI UpdateDriverNameEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_NAME'
  WHERE endpoint = 'DriverAPI UpdateDriverNameEndpoint';

-- {"api":"PostDriverUpdateName","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_NAME'
  WHERE endpoint = 'DriverAPI PostDriverUpdateNameEndpoint';

-- {"api":"PostDriverDeleteRC","migration":"endpoint","param":"DriverAPI DeleteRCEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_DELETE_RC'
  WHERE endpoint = 'DriverAPI DeleteRCEndpoint';

-- {"api":"PostDriverDeleteRC","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_DELETE_RC'
  WHERE endpoint = 'DriverAPI PostDriverDeleteRCEndpoint';

-- {"api":"PostDriverSendDummyNotification","migration":"endpoint","param":"DriverAPI SendDummyRideRequestToDriverViaDashboardEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_SEND_DUMMY_NOTIFICATION'
  WHERE endpoint = 'DriverAPI SendDummyRideRequestToDriverViaDashboardEndPoint';

-- {"api":"PostDriverSendDummyNotification","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_SEND_DUMMY_NOTIFICATION'
  WHERE endpoint = 'DriverAPI PostDriverSendDummyNotificationEndpoint';

-- {"api":"PostDriverChangeOperatingCity","migration":"endpoint","param":"DriverAPI ChangeOperatingCityEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_CHANGE_OPERATING_CITY'
  WHERE endpoint = 'DriverAPI ChangeOperatingCityEndpoint';

-- {"api":"PostDriverChangeOperatingCity","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_CHANGE_OPERATING_CITY'
  WHERE endpoint = 'DriverAPI PostDriverChangeOperatingCityEndpoint';

-- {"api":"PostDriverPauseOrResumeServiceCharges","migration":"endpoint","param":"DriverAPI PauseOrResumeServiceChargesEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_PAUSE_OR_RESUME_SERVICE_CHARGES'
  WHERE endpoint = 'DriverAPI PauseOrResumeServiceChargesEndPoint';

-- {"api":"PostDriverPauseOrResumeServiceCharges","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_PAUSE_OR_RESUME_SERVICE_CHARGES'
  WHERE endpoint = 'DriverAPI PostDriverPauseOrResumeServiceChargesEndpoint';

-- {"api":"PostDriverUpdateRCInvalidStatus","migration":"endpoint","param":"DriverAPI UpdateRCInvalidStatusEndPoint65454","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_RC_INVALID_STATUS'
  WHERE endpoint = 'DriverAPI UpdateRCInvalidStatusEndPoint65454';

-- {"api":"PostDriverUpdateRCInvalidStatus","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_RC_INVALID_STATUS'
  WHERE endpoint = 'DriverAPI PostDriverUpdateRCInvalidStatusEndpoint';

-- {"api":"PostDriverUpdateVehicleVariant","migration":"endpoint","param":"DriverAPI UpdateVehicleVariantEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_VEHICLE_VARIANT'
  WHERE endpoint = 'DriverAPI UpdateVehicleVariantEndPoint';

-- {"api":"PostDriverUpdateVehicleVariant","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_VEHICLE_VARIANT'
  WHERE endpoint = 'DriverAPI PostDriverUpdateVehicleVariantEndpoint';

-- {"api":"PostDriverBulkReviewRCVariant","migration":"endpoint","param":"DriverAPI BulkReviewRCVariantEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BULK_REVIEW_RC_VARIANT'
  WHERE endpoint = 'DriverAPI BulkReviewRCVariantEndPoint';

-- {"api":"PostDriverBulkReviewRCVariant","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BULK_REVIEW_RC_VARIANT'
  WHERE endpoint = 'DriverAPI PostDriverBulkReviewRCVariantEndpoint';

-- {"api":"PostDriverUpdateDriverTag","migration":"endpoint","param":"DriverAPI UpdateDriverTagEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_DRIVER_TAG'
  WHERE endpoint = 'DriverAPI UpdateDriverTagEndPoint';

-- {"api":"PostDriverUpdateDriverTag","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_DRIVER_TAG'
  WHERE endpoint = 'DriverAPI PostDriverUpdateDriverTagEndpoint';

-- {"api":"PostDriverClearFee","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_CLEAR_FEE'
  WHERE endpoint = 'DriverAPI PostDriverClearFeeEndpoint';

-- {"api":"PostDriverSyncDocAadharPan","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_SYNC_DOC_AADHAR_PAN'
  WHERE endpoint = 'DriverAPI PostDriverSyncDocAadharPanEndpoint';


------- SQL updates -------

-- {"api":"GetDriverDriverLicenseDetails","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_DRIVER_LICENSE_DETAILS'
  WHERE endpoint = 'DriverAPI GetDriverDriverLicenseDetailsEndpoint';

-- {"api":"GetDriverSearchRequests","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_SEARCH_REQUESTS'
  WHERE endpoint = 'DriverAPI GetDriverSearchRequestsEndpoint';
