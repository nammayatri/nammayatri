-- {"api":"PostDriverRegistrationDocumentUpload","migration":"endpoint","param":"DriverRegistrationAPI UploadDocumentEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DOCUMENT_UPLOAD'
  WHERE endpoint = 'DriverRegistrationAPI UploadDocumentEndpoint';

-- {"api":"PostDriverRegistrationDocumentUpload","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DOCUMENT_UPLOAD'
  WHERE endpoint = 'DriverRegistrationAPI PostDriverRegistrationDocumentUploadEndpoint';

-- {"api":"PostDriverRegistrationRegisterDl","migration":"endpoint","param":"DriverRegistrationAPI RegisterDLEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_DL'
  WHERE endpoint = 'DriverRegistrationAPI RegisterDLEndpoint';

-- {"api":"PostDriverRegistrationRegisterDl","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_DL'
  WHERE endpoint = 'DriverRegistrationAPI PostDriverRegistrationRegisterDlEndpoint';

-- {"api":"PostDriverRegistrationRegisterRc","migration":"endpoint","param":"DriverRegistrationAPI RegisterRCEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_RC'
  WHERE endpoint = 'DriverRegistrationAPI RegisterRCEndpoint';

-- {"api":"PostDriverRegistrationRegisterRc","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_RC'
  WHERE endpoint = 'DriverRegistrationAPI PostDriverRegistrationRegisterRcEndpoint';

-- {"api":"PostDriverRegistrationRegisterGenerateAadhaarOtp","migration":"endpoint","param":"DriverRegistrationAPI GenerateAadhaarOtpEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_GENERATE_AADHAAR_OTP'
  WHERE endpoint = 'DriverRegistrationAPI GenerateAadhaarOtpEndpoint';

-- {"api":"PostDriverRegistrationRegisterGenerateAadhaarOtp","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_GENERATE_AADHAAR_OTP'
  WHERE endpoint = 'DriverRegistrationAPI PostDriverRegistrationRegisterGenerateAadhaarOtpEndpoint';

-- {"api":"PostDriverRegistrationRegisterVerifyAadhaarOtp","migration":"endpoint","param":"DriverRegistrationAPI VerifyAadhaarOtpEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_VERIFY_AADHAAR_OTP'
  WHERE endpoint = 'DriverRegistrationAPI VerifyAadhaarOtpEndpoint';

-- {"api":"PostDriverRegistrationRegisterVerifyAadhaarOtp","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_VERIFY_AADHAAR_OTP'
  WHERE endpoint = 'DriverRegistrationAPI PostDriverRegistrationRegisterVerifyAadhaarOtpEndpoint';

-- {"api":"PostDriverRegistrationDocumentsUpdate","migration":"endpoint","param":"DriverRegistrationAPI UpdateDocumentEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DOCUMENTS_UPDATE'
  WHERE endpoint = 'DriverRegistrationAPI UpdateDocumentEndpoint';

-- {"api":"PostDriverRegistrationDocumentsUpdate","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DOCUMENTS_UPDATE'
  WHERE endpoint = 'DriverRegistrationAPI PostDriverRegistrationDocumentsUpdateEndpoint';


------- SQL updates -------

-- {"api":"GetDriverRegistrationDocumentsList","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS DOCUMENT_LIST","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_DOCUMENTS_LIST' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'DOCUMENT_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverRegistrationGetDocument","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS GET_DOCUMENT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_GET_DOCUMENT' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'GET_DOCUMENT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverRegistrationDocumentUpload","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS UPLOAD_DOCUMENT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DOCUMENT_UPLOAD' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'UPLOAD_DOCUMENT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverRegistrationRegisterDl","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS REGISTER_DL","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_DL' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'REGISTER_DL' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverRegistrationRegisterRc","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS REGISTER_RC","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_RC' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'REGISTER_RC' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverRegistrationRegisterGenerateAadhaarOtp","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS GENERATE_AADHAAR_OTP","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_GENERATE_AADHAAR_OTP' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'GENERATE_AADHAAR_OTP' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverRegistrationRegisterVerifyAadhaarOtp","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS VERIFY_AADHAAR_OTP","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_VERIFY_AADHAAR_OTP' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'VERIFY_AADHAAR_OTP' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverRegistrationUnderReviewDrivers","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS UNDER_REVIEW_DRIVERS_LIST","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_UNDER_REVIEW_DRIVERS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'UNDER_REVIEW_DRIVERS_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverRegistrationDocumentsInfo","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS DRIVER_DOCUMENT_INFO","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_DOCUMENTS_INFO' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'DRIVER_DOCUMENT_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverRegistrationDocumentsUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS UPDATE_DOCUMENT","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DOCUMENTS_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'UPDATE_DOCUMENT' ) ON CONFLICT DO NOTHING;
