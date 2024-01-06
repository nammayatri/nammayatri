ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN special_drivers type text[] default '{}';
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN special_location_tags type text[] default '{}';

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN is_free_ride type boolean;
