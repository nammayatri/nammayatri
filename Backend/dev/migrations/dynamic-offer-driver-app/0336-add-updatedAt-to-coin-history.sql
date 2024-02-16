ALTER TABLE atlas_driver_offer_bpp.coin_history
ADD COLUMN updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now();
