
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN onboarding_try_limit SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN onboarding_retry_time_in_hours SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN check_image_extraction_for_dashboard SET NOT NULL;

CREATE TABLE atlas_driver_offer_bpp.onboarding_document_configs (
merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
document_type TEXT NOT NULL,
check_extraction BOOLEAN NOT NULL,
check_expiry BOOLEAN NOT NULL,
valid_vehicle_classes TEXT[] NOT NULL,
vehicle_class_check_type TEXT NOT NULL ,
CONSTRAINT PK_onboarding_document_configs PRIMARY KEY (merchant_id , document_type)
);