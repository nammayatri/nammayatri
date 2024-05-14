-- don’t run these queries in master & prod.
INSERT INTO atlas_app.frfs_config (booking_end_time, booking_start_time, merchant_id, merchant_operating_city_id, one_way_ticket_limit, round_trip_ticket_limit, metro_station_ttl, discount, custom_end_time,custom_dates, created_at, updated_at)
VALUES ('2022-04-12 17:00:00.151585+00', '2022-04-11 23:00:00.151585+00', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-0000-00000000city', 6, 6, 10080, 25, '', ARRAY[''], NOW(), NOW());
