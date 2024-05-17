ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_earnings int DEFAULT 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN bonus_earned int DEFAULT 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN late_night_trips int DEFAULT 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN earnings_missed int DEFAULT 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
