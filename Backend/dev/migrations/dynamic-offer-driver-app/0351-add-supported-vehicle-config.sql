alter table atlas_driver_offer_bpp.transporter_config add column supported_vehicles text[] default ARRAY['AUTO_RICKSHAW']; -- need to run update query

--
update atlas_driver_offer_bpp.transporter_config
set supported_vehicles = ARRAY['TAXI', 'TAXI_PLUS', 'SEDAN']
where merchant_operating_city_id = 'PUT KOLKATA MERCHANT OPCITY ID';

--
update atlas_driver_offer_bpp.transporter_config
set supported_vehicles = ARRAY['SEDAN', 'SUV', 'AUTO_RICKSHAW', 'HATCHBACK']
where merchant_operating_city_id = 'PUT KOCHI MERCHANT OPCITY ID';

--
update atlas_driver_offer_bpp.transporter_config
set supported_vehicles = ARRAY['SEDAN', 'SUV', 'HATCHBACK']
where merchant_operating_city_id = 'PUT CHENNAI MERCHANT OPCITY ID';
