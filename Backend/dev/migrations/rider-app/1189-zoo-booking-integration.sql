INSERT INTO atlas_app.ticket_place (id, merchant_operating_city_id, name, description, lat, lon, gallery, open_timings, close_timings) VALUES ('1ef78db2-90de-4ed7-a38a-fcbb7ce28135', '7ef7af35-19be-9f28-9330-b346ed992a1d', 'Alipore Zoo',
    'The Zoological Garden, Alipore is Indias oldest formally stated zoological park (as opposed to royal and British menageries) and a big tourist attraction in Kolkata, West Bengal.',
    22.537351383029193,88.33227051149596, '{}', '03:30:00', '12:00:00');

INSERT INTO atlas_app.ticket_service (id, places_id, service, max_verification, business_hours) VALUES ('b73378dc-427f-4efa-9b55-8efe7e3352c2', '1ef78db2-90de-4ed7-a38a-fcbb7ce28135', 'Entrance Fee', 2, '{}');

INSERT INTO atlas_app.ticket_service (id, places_id, service, max_verification, business_hours) VALUES ('a7eba6ed-99f7-442f-a9d8-00c8b380657b', '1ef78db2-90de-4ed7-a38a-fcbb7ce28135', 'Aquarium Fee', 2, '{}');

INSERT INTO atlas_app.ticket_service (id, places_id, service, max_verification, business_hours) VALUES ('d8f47b42-50a5-4a97-8dda-e80a3633d7ab', '1ef78db2-90de-4ed7-a38a-fcbb7ce28135', 'Videography Fee', 2, '{}');
