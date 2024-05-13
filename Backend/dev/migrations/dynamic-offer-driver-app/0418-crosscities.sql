ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cross_travel_cities text[];

update atlas_driver_offer_bpp.transporter_config
set cross_travel_cities = '{Delhi, Gurugram}';
