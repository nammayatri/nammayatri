ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN fav_rider_count Int DEFAULT '0';
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN fav_rider_list Text[] DEFAULT '{}';
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN fav_driver_list Text[] Not Null DEFAULT '{}';