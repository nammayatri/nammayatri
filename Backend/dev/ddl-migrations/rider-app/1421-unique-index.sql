CREATE INDEX idx_route_code_stop_code_integrated_bpp_config_id on  atlas_app.route_stop_time_table  using btree (route_code, stop_code, integrated_bpp_config_id);

create index idx_route_code_integrated_bpp_config_id on atlas_app.route using btree (code, integrated_bpp_config_id);

create index idx_station_code_integrated_bpp_config_id on atlas_app.station using btree (code, integrated_bpp_config_id);

create index idx_frfs_route_fare_product_code_integrated_bpp_config_id on atlas_app.frfs_route_fare_product using btree (route_code, integrated_bpp_config_id);