-- ONLY RUN IN MASTER -- NOT IN PROD
update atlas_driver_offer_bpp.document_verification_config
  set dependency_document_type = '{}'
  where document_type = 'VehicleRegistrationCertificate' and do_strict_verifcation = true