ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN referral_link_password text;
UPDATE atlas_driver_offer_bpp.transporter_config SET referral_link_password='1234567890';
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN referral_link_password SET NOT NULL;
