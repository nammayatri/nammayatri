--ONLY LOCAL SYNC
UPDATE atlas_driver_offer_bpp.transporter_config SET onboarding_try_limit=3;
UPDATE atlas_driver_offer_bpp.transporter_config SET onboarding_retry_time_in_hours=24;
UPDATE atlas_driver_offer_bpp.transporter_config SET check_image_extraction_for_dashboard=true;
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

WITH MerchantMessages AS (
  SELECT T1.id, 'ONBOARD_SUPPORT_SMS_TEMPLATE', ' Driver Onboarding Alert!!
           Driver is facing following issues while onboarding to ({#org#}).
          Reasons:
           {#reasons#}
          Please contact him +91-{#driver-phone#}.'
  FROM atlas_driver_offer_bpp.merchant AS T1
)
INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message)
  (SELECT * FROM MerchantMessages);

INSERT INTO atlas_driver_offer_bpp.onboarding_document_configs VALUES ('favorit0-0000-0000-0000-00000favorit', 'DL', false , true,ARRAY[ 'AUTORICKSHAW', 'LMV', '3W-NT', '3WT', '3W-T', 'LIGHT MOTOR VEHICLE', '3W-CAB' ], 'Infix' ) ;
INSERT INTO atlas_driver_offer_bpp.onboarding_document_configs VALUES ('favorit0-0000-0000-0000-00000favorit', 'RC', false , false,ARRAY['3WT','Passenger','3WN'], 'Infix' ) ;
INSERT INTO atlas_driver_offer_bpp.onboarding_document_configs VALUES ('favorit0-0000-0000-0000-00000favorit', 'RCInsurance', false , false,ARRAY[]::TEXT[] , 'Infix' ) ;