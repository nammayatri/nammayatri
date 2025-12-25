ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs ADD COLUMN rc_number_prefix text not null default 'KA';

ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs ADD COLUMN supported_vehicle_classes_json json;
-- have to write queries here
update atlas_driver_offer_bpp.onboarding_document_configs set supported_vehicle_classes_json =
    json_build_array('AUTORICKSHAW', 'LMV', '3W-NT', '3WT', '3W-T', 'LIGHT MOTOR VEHICLE', '3W-CAB', 'ARNT') where document_type = 'DL';

update atlas_driver_offer_bpp.onboarding_document_configs set supported_vehicle_classes_json =
    json_build_array(json_build_object('vehicleClass', '3WT', 'vehicleVariant', 'AUTO_RICKSHAW'),
    json_build_object('vehicleClass', 'Passenger', 'vehicleCapacity', 4, 'vehicleVariant', 'AUTO_RICKSHAW'),
    json_build_object('vehicleClass', 'Quadricycle', 'vehicleCapacity', 4, 'vehicleVariant', 'AUTO_RICKSHAW'),
    json_build_object('vehicleClass', '3WN', 'vehicleVariant', 'AUTO_RICKSHAW')) where document_type = 'RC';

update atlas_driver_offer_bpp.onboarding_document_configs set supported_vehicle_classes_json =
    json_build_array() where document_type = 'RCInsurance';

ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs ALTER COLUMN supported_vehicle_classes_json set not null;
-------------------------------------------------------------------------------------------
-------------------------------DROPS-------------------------------------------------------
-------------------------------------------------------------------------------------------
ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs DROP COLUMN valid_vehicle_classes;
