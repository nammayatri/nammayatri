ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN referral_code character varying(15);
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN referred_by_driver character varying(255);
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN referred_at timestamp with time zone;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN has_taken_ride Boolean;
UPDATE atlas_driver_offer_bpp.rider_details SET has_taken_ride = false;
UPDATE atlas_driver_offer_bpp.rider_details SET has_taken_ride = true WHERE id in (SELECT rider_id FROM atlas_driver_offer_bpp.booking WHERE status = 'COMPLETED');
ALTER TABLE atlas_driver_offer_bpp.rider_details ALTER COLUMN has_taken_ride SET NOT NULL;
