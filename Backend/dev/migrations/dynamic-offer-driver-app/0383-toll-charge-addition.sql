-- For Bangalore disable isAvoidToll on BAP and Keep this flag as false to exclude autos from Estimates on toll based routes
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN allow_autos_on_toll_route bool NOT NULL DEFAULT true;

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN toll_charges double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN toll_charges double precision;
