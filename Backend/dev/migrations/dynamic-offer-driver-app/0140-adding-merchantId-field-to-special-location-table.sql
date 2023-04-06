ALTER TABLE atlas_driver_offer_bpp.special_location ADD COLUMN merchant_id character(36) REFERENCES atlas_driver_offer_bpp.merchant (id);

UPDATE atlas_driver_offer_bpp.special_location SET merchant_id = 'favorit0-0000-0000-0000-00000favorit';

ALTER TABLE atlas_driver_offer_bpp.special_location ALTER COLUMN merchant_id SET NOT NULL;
