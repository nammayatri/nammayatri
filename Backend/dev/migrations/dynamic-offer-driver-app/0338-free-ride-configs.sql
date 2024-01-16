ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN special_drivers text[] default '{}';
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN special_location_tags text[] default '{}';

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN is_free_ride boolean;
