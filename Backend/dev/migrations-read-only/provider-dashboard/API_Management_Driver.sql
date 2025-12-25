-- {"api":"PostDriverPersonNumbers","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_PERSON_NUMBERS'
  WHERE endpoint = 'DriverAPI PostDriverPersonNumbersEndpoint';

-- {"api":"PostDriverPersonId","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_PERSON_ID'
  WHERE endpoint = 'DriverAPI PostDriverpersonIdEndpoint';

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

-- {"api":"GetDriverDocumentsInfo","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS DOCUMENTS_INFO","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_DOCUMENTS_INFO' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'DOCUMENTS_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverPersonNumbers","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS PERSON_NUMBERS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_PERSON_NUMBERS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'PERSON_NUMBERS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverDriverDataDecryption","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS DRIVER_DECRYPTION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_DRIVER_DATA_DECRYPTION' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'DRIVER_DECRYPTION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverPersonId","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS FETCH_PERSON_ID","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_PERSON_ID' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'FETCH_PERSON_ID' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverAadhaarInfo","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS AADHAAR_INFO","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_AADHAAR_INFO' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'AADHAAR_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverAadhaarInfobyMobileNumber","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS AADHAAR_INFO_PHONE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_AADHAAR_INFOBY_MOBILE_NUMBER' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'AADHAAR_INFO_PHONE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverList","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS LIST","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_LIST' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverActivity","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS ACTIVITY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_ACTIVITY' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'ACTIVITY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverDisable","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS DISABLE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_DISABLE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'DISABLE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverAcRestrictionUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS REMOVE_AC_USAGE_RESTRICTION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_AC_RESTRICTION_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'REMOVE_AC_USAGE_RESTRICTION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverBlockWithReason","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS BLOCK_WITH_REASON","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BLOCK_WITH_REASON' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'BLOCK_WITH_REASON' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverBlock","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS BLOCK","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BLOCK' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'BLOCK' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverBlockReasonList","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS BLOCK_REASON_LIST","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_BLOCK_REASON_LIST' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'BLOCK_REASON_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverUnblock","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS UNBLOCK","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNBLOCK' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'UNBLOCK' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverLocation","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS LOCATION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_LOCATION' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'LOCATION' ) ON CONFLICT DO NOTHING;

-- {"api":"DeleteDriverPermanentlyDelete","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS DELETE_DRIVER","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/DELETE_DRIVER_PERMANENTLY_DELETE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'DELETE_DRIVER' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverUnlinkDL","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS UNLINK_DL","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNLINK_DL' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'UNLINK_DL' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverUnlinkAadhaar","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS UNLINK_AADHAAR","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNLINK_AADHAAR' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'UNLINK_AADHAAR' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverUpdatePhoneNumber","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS UPDATE_PHONE_NUMBER","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_PHONE_NUMBER' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'UPDATE_PHONE_NUMBER' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverUpdateByPhoneNumber","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS AADHAAR_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_BY_PHONE_NUMBER' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'AADHAAR_UPDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverUpdateName","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS UPDATE_DRIVER_NAME","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_NAME' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'UPDATE_DRIVER_NAME' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverDeleteRC","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS DELETE_RC","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_DELETE_RC' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'DELETE_RC' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverClearStuckOnRide","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS CLEAR_ON_RIDE_STUCK_DRIVER_IDS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_CLEAR_STUCK_ON_RIDE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'CLEAR_ON_RIDE_STUCK_DRIVER_IDS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverSendDummyNotification","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS SEND_DUMMY_NOTIFICATION","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_SEND_DUMMY_NOTIFICATION' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'SEND_DUMMY_NOTIFICATION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverChangeOperatingCity","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS CHANGE_OPERATING_CITY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_CHANGE_OPERATING_CITY' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'CHANGE_OPERATING_CITY' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverGetOperatingCity","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS GET_OPERATING_CITY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_GET_OPERATING_CITY' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'GET_OPERATING_CITY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverPauseOrResumeServiceCharges","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS TOGGLE_SERVICE_USAGE_CHARGE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_PAUSE_OR_RESUME_SERVICE_CHARGES' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'TOGGLE_SERVICE_USAGE_CHARGE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverUpdateRCInvalidStatus","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS UPDATE_RC_INVALID_STATUS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_RC_INVALID_STATUS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'UPDATE_RC_INVALID_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverUpdateVehicleVariant","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS UPDATE_VEHICLE_VARIANT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_VEHICLE_VARIANT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'UPDATE_VEHICLE_VARIANT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverBulkReviewRCVariant","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS BULK_REVIEW_RC_VARIANT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BULK_REVIEW_RC_VARIANT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'BULK_REVIEW_RC_VARIANT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverUpdateDriverTag","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS UPDATE_DRIVER_TAG","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_DRIVER_TAG' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'UPDATE_DRIVER_TAG' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverClearFee","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS CLEAR_FEE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_CLEAR_FEE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'CLEAR_FEE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverPanAadharSelfieDetails","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS PAN_AADHAAR_SELFIE_DETAILS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_PAN_AADHAR_SELFIE_DETAILS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'PAN_AADHAAR_SELFIE_DETAILS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverSyncDocAadharPan","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS SYNC_DOC_AADHAR_PAN","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_SYNC_DOC_AADHAR_PAN' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'SYNC_DOC_AADHAR_PAN' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverUpdateVehicleManufacturing","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS UPDATE_VEHICLE_MANUFACTURING","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_VEHICLE_MANUFACTURING' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'UPDATE_VEHICLE_MANUFACTURING' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverRefundByPayout","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS REFUND_BY_PAYOUT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_REFUND_BY_PAYOUT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'REFUND_BY_PAYOUT' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverSecurityDepositStatus","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS SECURITY_DEPOSIT_STATUS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_SECURITY_DEPOSIT_STATUS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'SECURITY_DEPOSIT_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverPanAadharSelfieDetailsList","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS PAN_AADHAAR_SELFIE_DETAILS_LIST","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_PAN_AADHAR_SELFIE_DETAILS_LIST' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'PAN_AADHAAR_SELFIE_DETAILS_LIST' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostDriverBulkSubscriptionServiceUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS BULK_SERVICE_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BULK_SUBSCRIPTION_SERVICE_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'BULK_SERVICE_UPDATE' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetDriverStats","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS STATS","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_STATS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'STATS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostDriverUpdateTagBulk","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_TAG_BULK'
  WHERE endpoint = 'DriverAPI PostDriverUpdateTagBulkEndpoint';

-- {"api":"PostDriverUpdateTagBulk","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS UPDATE_TAG_BULK","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_TAG_BULK' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'UPDATE_TAG_BULK' ) ON CONFLICT DO NOTHING;
