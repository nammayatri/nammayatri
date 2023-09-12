ALTER TABLE atlas_driver_offer_bpp.driver_information
ADD COLUMN comp_aadhaar_image_path TEXT;

ALTER TABLE atlas_driver_offer_bpp.transporter_config
ADD COLUMN aadhaar_image_resize_config JSON;

UPDATE atlas_driver_offer_bpp.transporter_config
SET aadhaar_image_resize_config = '{"height": 120, "width": 120}'::json
WHERE merchant_id = 'favorit0-0000-0000-0000-00000favorit';

ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification
ADD COLUMN driver_image_path Text;