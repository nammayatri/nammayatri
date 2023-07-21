ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_earnings int DEFAULT 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN bonus_earned int DEFAULT 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN late_night_trips int DEFAULT 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN earnings_missed int DEFAULT 0;

ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN hometown character varying(255);
ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN languages_spoken text[] DEFAULT '{}'::text[];

ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN vehicle_name character varying(255);