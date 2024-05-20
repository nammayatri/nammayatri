ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN rides_cancelled int;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_rides_assigned int;

update atlas_driver_offer_bpp.transporter_config set threshold_cancellation_percentage_to_unlist = 70;
update atlas_driver_offer_bpp.transporter_config set min_rides_to_unlist = 10;
