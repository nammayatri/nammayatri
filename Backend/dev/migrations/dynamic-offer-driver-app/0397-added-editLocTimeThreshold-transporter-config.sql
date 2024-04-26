ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN edit_loc_time_threshold bigint default 120 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN edit_loc_driver_permission_needed boolean default true NOT NULL;

