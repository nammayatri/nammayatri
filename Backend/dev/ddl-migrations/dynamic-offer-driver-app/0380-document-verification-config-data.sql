

-- ONLY RUN THESE AFTER UI RELEASE
-- update atlas_driver_offer_bpp.document_verification_config set is_mandatory = true where document_type in ('VehiclePermit', 'VehiclePUC', 'VehicleInsurance', 'VehicleFitnessCertificate') and vehicle_category = 'CAR';

-- update atlas_driver_offer_bpp.document_verification_config set is_mandatory = true where document_type in ('PanCard', 'AadhaarCard', 'ProfilePhoto') and vehicle_category = 'CAR';

-- RUN IT AFTER THE RELEASE
DROP TABLE atlas_driver_offer_bpp.onboarding_document_configs;

  -- | SubscriptionPlan
  -- | ProfilePhoto
  -- | AadhaarCard
  -- | PanCard
  -- | NO_OPTION