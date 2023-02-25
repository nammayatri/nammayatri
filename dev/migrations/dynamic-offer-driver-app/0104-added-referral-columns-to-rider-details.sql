ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN referral_code character varying(15);
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN referred_by_driver character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN referred_at timestamp with time zone;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN has_taken_ride Boolean not null default false;
