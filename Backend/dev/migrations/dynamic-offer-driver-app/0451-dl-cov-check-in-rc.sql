-- RUN IN MASTER AND PROD

update atlas_driver_offer_bpp.document_verification_config
  set dependency_document_type = '{DriverLicense}'
  where document_type = 'VehicleRegistrationCertificate' and do_strict_verifcation = true