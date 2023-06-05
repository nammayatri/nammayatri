ALTER TABLE atlas_driver_offer_bpp.message
ADD COLUMN short_description Text DEFAULT '';

ALTER TABLE atlas_driver_offer_bpp.message_translation
ADD COLUMN short_description Text DEFAULT '';