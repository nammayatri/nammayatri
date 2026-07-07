-- Enable mark_image_valid_on_validation_skip for MSIL_PARTNER Delhi.
-- When true, image validation is skipped and the image is directly marked VALID.
UPDATE atlas_driver_offer_bpp.document_verification_config
SET mark_image_valid_on_validation_skip = true
WHERE merchant_operating_city_id = 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e';

UPDATE atlas_driver_offer_bpp.fleet_owner_document_verification_config
SET mark_image_valid_on_validation_skip = true
WHERE merchant_operating_city_id = 'db1c62db-01ba-4cbf-9cce-ede8d2e1361e';
