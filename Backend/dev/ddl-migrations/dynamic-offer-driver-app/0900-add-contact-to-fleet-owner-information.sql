-- How the fleet itself can be reached, apart from the owner's own phone and email. Payments,
-- payouts and alerts read these. They are not a login and not a lookup key. Empty means fall back
-- to the owner's own phone and email, which is what every existing fleet does.
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN IF NOT EXISTS contact_mobile_country_code character varying (8);
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN IF NOT EXISTS contact_mobile_number_encrypted character varying (255);
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN IF NOT EXISTS contact_mobile_number_hash bytea;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN IF NOT EXISTS contact_email_encrypted character varying (255);
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN IF NOT EXISTS contact_email_hash bytea;
