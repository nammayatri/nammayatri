ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN estimate_id text REFERENCES atlas_driver_offer_bpp.estimate(id);
