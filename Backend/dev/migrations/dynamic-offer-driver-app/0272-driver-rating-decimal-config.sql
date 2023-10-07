-- Can be set to true for Namma Yatri & Yatri
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN rating_as_decimal boolean NOT NULL DEFAULT false;