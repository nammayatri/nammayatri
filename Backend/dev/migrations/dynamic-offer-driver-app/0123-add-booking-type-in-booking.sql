ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN booking_type character(36) DEFAULT 'NormalBooking' NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN special_zone_otp_code character(4);