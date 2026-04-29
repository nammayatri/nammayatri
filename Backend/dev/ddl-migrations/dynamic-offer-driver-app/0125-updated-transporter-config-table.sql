
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN pickup_loc_threshold SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN drop_loc_threshold SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN ride_time_estimated_threshold SET NOT NULL;