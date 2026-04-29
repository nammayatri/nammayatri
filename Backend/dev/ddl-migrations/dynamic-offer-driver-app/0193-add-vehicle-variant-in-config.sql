ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs ADD COLUMN rc_number_prefix text not null default 'KA';

ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs ADD COLUMN supported_vehicle_classes_json json;

ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs ALTER COLUMN supported_vehicle_classes_json set not null;
-------------------------------------------------------------------------------------------
-------------------------------DROPS-------------------------------------------------------
-------------------------------------------------------------------------------------------
ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs DROP COLUMN valid_vehicle_classes;
