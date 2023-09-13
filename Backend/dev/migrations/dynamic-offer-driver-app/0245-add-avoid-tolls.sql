ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN is_avoid_toll BOOLEAN NOT NULL DEFAULT true ;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN special_zone_booking_otp_expiry Int NOT NULL DEFAULT 60;