update ticket_service set operational_days = '{Monday,Tuesday,Wednesday,Friday,Saturday,Sunday}', allow_future_booking = true, expiry = 'VisitDate 12:00:00';

update ticket_service set business_hour = '{b15378b5-0a86-44ce-afa1-b4d27d712a23}' where id = 'b73378dc-427f-4efa-9b55-8efe7e3352c2';
update ticket_service set business_hour = '{b25378b5-0a86-44ce-afa1-b4d27d712a23}' where id = 'a7eba6ed-99f7-442f-a9d8-00c8b380657b';
update ticket_service set business_hour = '{b35378b5-0a86-44ce-afa1-b4d27d712a23}' where id = 'd8f47b42-50a5-4a97-8dda-e80a3633d7ab';

insert business_hour (id, btype, category_id) values ('b15378b5-0a86-44ce-afa1-b4d27d712a23', 'Duration 03:30:00 11:30:00', '{125378b5-0a86-44ce-afa1-b4d27d712a23}');
insert business_hour (id, btype, category_id) values ('b2378b5-0a86-44ce-afa1-b4d27d712a23', 'Duration 03:30:00 11:30:00', '{125378b5-0a86-44ce-afa1-b4d27d712a23}');
insert business_hour (id, btype, category_id) values ('b3378b5-0a86-44ce-afa1-b4d27d712a23', 'Duration 05:00:00 11:00:00', '{125378b5-0a86-44ce-afa1-b4d27d712a23}');

insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (null, null, 'all', 'c15378b5-0a86-44ce-afa1-b4d27d712a23', 'all', '{125378b5-0a86-44ce-afa1-b4d27d712a23, 225378b5-0a86-44ce-afa1-b4d27d712a23}');
insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (null, null, 'all', 'c25378b5-0a86-44ce-afa1-b4d27d712a23', 'all', '{325378b5-0a86-44ce-afa1-b4d27d712a23, 425378b5-0a86-44ce-afa1-b4d27d712a23}');
insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (null, null, 'all', 'c35378b5-0a86-44ce-afa1-b4d27d712a23', 'all', '{525378b5-0a86-44ce-afa1-b4d27d712a23}');

insert into atlas_app.service_people_category (id, name, description, price_per_unit) values ('125378b5-0a86-44ce-afa1-b4d27d712a23', 'Adult', 'Adult', 50);
insert into atlas_app.service_people_category (id, name, description, price_per_unit) values ('225378b5-0a86-44ce-afa1-b4d27d712a23', 'Adult', 'Adult', 20);

insert into atlas_app.service_people_category (id, name, description, price_per_unit) values ('325378b5-0a86-44ce-afa1-b4d27d712a23', 'Adult', 'Adult', 20);
insert into atlas_app.service_people_category (id, name, description, price_per_unit) values ('425378b5-0a86-44ce-afa1-b4d27d712a23', 'Adult', 'Adult', 10);

insert into atlas_app.service_people_category (id, name, description, price_per_unit) values ('525378b5-0a86-44ce-afa1-b4d27d712a23', 'CameraUnit', 'CameraUnit', 250);

-- old bookings
insert into atlas_app.ticket_booking_service_category (id, name, amount, booked_seats, ticket_booking_service_id) values(select atlas_app.uuid_generate_v4() as id, 'all' as name, amount, null as booked_seats, id as ticket_booking_service_id from atlas_app.ticket_booking_service);

insert into atlas_app.ticket_booking_people_category (id, name, number_of_units, price_per_unit, ticket_booking_service_category_id) values (select atlas_app.uuid_generate_v4(), attendee_type as name, number_of_units, price_per_unit, ticket_booking_service_category.id as ticket_booking_service_category_id from ticket_booking_service_price_breakup left join ticket_booking_service_category on ticket_booking_service_category.ticket_booking_service_id = ticket_booking_service.id);
