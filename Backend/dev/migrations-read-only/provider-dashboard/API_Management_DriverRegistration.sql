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
