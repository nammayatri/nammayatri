ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs
ADD COLUMN rc_number_prefix_list text[];

UPDATE atlas_driver_offer_bpp.onboarding_document_configs
SET rc_number_prefix_list = ARRAY[rc_number_prefix];

