insert into atlas_app.route_polylines (id, route_id, polyline, created_at, updated_at, merchant_id, merchant_operating_city_id, vehicle_type)
(select gen_random_uuid() as id, code as route_id, polyline, now() as created_at, now() as updated_at, merchant_id, merchant_operating_city_id, vehicle_type from atlas_app.route
    where integrated_bpp_config_id in
    (select id from atlas_app.integrated_bpp_config));

insert into atlas_app.stations_extra_information (id, station_id, address, created_at, updated_at, merchant_id, merchant_operating_city_id)
(select gen_random_uuid() as id, code as station_id, address, now() as created_at, now() as updated_at, merchant_id, merchant_operating_city_id from atlas_app.station
    where integrated_bpp_config_id in
    (select id from atlas_app.integrated_bpp_config));
