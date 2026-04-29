ALTER TABLE atlas_driver_offer_bpp.gate_info
ADD COLUMN updated_at timestamp;

ALTER TABLE atlas_driver_offer_bpp.gate_info
ALTER COLUMN updated_at SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.special_location
ADD COLUMN updated_at timestamp;

ALTER TABLE atlas_driver_offer_bpp.special_location
ALTER COLUMN updated_at SET NOT NULL;