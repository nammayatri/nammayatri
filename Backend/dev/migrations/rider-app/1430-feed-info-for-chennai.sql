ALTER TABLE atlas_app.gtfs_feed_info
ADD CONSTRAINT unique_vehicle_merchant_city UNIQUE (vehicle_type, merchant_id, merchant_operating_city_id);

insert into atlas_app.gtfs_feed_info (feed_id, vehicle_type, merchant_id, merchant_operating_city_id) values ('chennai_bus', 'BUS', '4b17bd06-ae7e-48e9-85bf-282fb310209c', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874');
insert into atlas_app.gtfs_feed_info (feed_id, vehicle_type, merchant_id, merchant_operating_city_id) values ('chennai_metro', 'METRO', '4b17bd06-ae7e-48e9-85bf-282fb310209c', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874');
insert into atlas_app.gtfs_feed_info (feed_id, vehicle_type, merchant_id, merchant_operating_city_id) values ('chennai_subway', 'SUBWAY', '4b17bd06-ae7e-48e9-85bf-282fb310209c', 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874');
