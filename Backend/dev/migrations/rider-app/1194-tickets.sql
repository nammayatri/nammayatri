DROP TABLE IF EXISTS atlas_app.business_hour;

CREATE TABLE atlas_app.business_hour ();

ALTER TABLE atlas_app.business_hour ADD COLUMN btype text NOT NULL;
ALTER TABLE atlas_app.business_hour ADD COLUMN category_id text[] NOT NULL;
ALTER TABLE atlas_app.business_hour ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.business_hour ADD PRIMARY KEY ( id);DROP TABLE IF EXISTS atlas_app.seat_management;

CREATE TABLE atlas_app.seat_management ();

ALTER TABLE atlas_app.seat_management ADD COLUMN blocked integer NOT NULL;
ALTER TABLE atlas_app.seat_management ADD COLUMN booked integer NOT NULL;
ALTER TABLE atlas_app.seat_management ADD COLUMN date date NOT NULL;
ALTER TABLE atlas_app.seat_management ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.seat_management ADD COLUMN ticket_service_category_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.seat_management ADD PRIMARY KEY ( id);DROP TABLE IF EXISTS atlas_app.service_category;

CREATE TABLE atlas_app.service_category ();

ALTER TABLE atlas_app.service_category ADD COLUMN allowed_seats integer ;
ALTER TABLE atlas_app.service_category ADD COLUMN available_seats integer ;
ALTER TABLE atlas_app.service_category ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.service_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.service_category ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.service_category ADD COLUMN people_category text[] ;
ALTER TABLE atlas_app.service_category ADD PRIMARY KEY ( id);DROP TABLE IF EXISTS atlas_app.service_people_category;

CREATE TABLE atlas_app.service_people_category ();

ALTER TABLE atlas_app.service_people_category ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.service_people_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.service_people_category ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.service_people_category ADD COLUMN price_per_unit double precision NOT NULL;
ALTER TABLE atlas_app.service_people_category ADD PRIMARY KEY ( id);DROP TABLE IF EXISTS atlas_app.special_occasion;

CREATE TABLE atlas_app.special_occasion ();

ALTER TABLE atlas_app.special_occasion ADD COLUMN business_hours text[] NOT NULL;
ALTER TABLE atlas_app.special_occasion ADD COLUMN date date ;
ALTER TABLE atlas_app.special_occasion ADD COLUMN day_of_week text ;
ALTER TABLE atlas_app.special_occasion ADD COLUMN description text ;
ALTER TABLE atlas_app.special_occasion ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_app.special_occasion ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.special_occasion ADD COLUMN special_day_type date NOT NULL;
ALTER TABLE atlas_app.special_occasion ADD PRIMARY KEY ( id);DROP TABLE IF EXISTS atlas_app.ticket_booking;

CREATE TABLE atlas_app.ticket_booking ();

ALTER TABLE atlas_app.ticket_booking ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN created_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN ticket_place_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN updated_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN visit_date date NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD PRIMARY KEY ( id, person_id, short_id);DROP TABLE IF EXISTS atlas_app.ticket_booking_people_category;

CREATE TABLE atlas_app.ticket_booking_people_category ();

ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN name text ;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN number_of_units integer ;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN price_per_unit double precision ;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN ticket_booking_service_category_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking_people_category ADD PRIMARY KEY ( id);DROP TABLE IF EXISTS atlas_app.ticket_booking_service;

CREATE TABLE atlas_app.ticket_booking_service ();

ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN btype text NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN created_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN expiry_date timestamp with time zone ;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN ticket_booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN ticket_service_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN updated_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN verification_count integer NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD PRIMARY KEY ( id);DROP TABLE IF EXISTS atlas_app.ticket_booking_service_category;

CREATE TABLE atlas_app.ticket_booking_service_category ();

ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN amount double precision ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN booked_seats integer ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN name text ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN ticket_booking_service_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD PRIMARY KEY ( id);DROP TABLE IF EXISTS atlas_app.ticket_place;

CREATE TABLE atlas_app.ticket_place ();

ALTER TABLE atlas_app.ticket_place ADD COLUMN close_timings time without time zone ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN description text ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN gallery text[] NOT NULL;
ALTER TABLE atlas_app.ticket_place ADD COLUMN icon_url text ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_place ADD COLUMN lat double precision ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN lon double precision ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN map_image_url text ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.ticket_place ADD COLUMN open_timings time without time zone ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN place_type text ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN short_desc text NOT NULL;
ALTER TABLE atlas_app.ticket_place ADD COLUMN terms_and_conditions text[] NOT NULL;
ALTER TABLE atlas_app.ticket_place ADD PRIMARY KEY ( id);DROP TABLE IF EXISTS atlas_app.ticket_service;

CREATE TABLE atlas_app.ticket_service ();

ALTER TABLE atlas_app.ticket_service ADD COLUMN allow_future_booking boolean ;
ALTER TABLE atlas_app.ticket_service ADD COLUMN business_hours text[] NOT NULL;
ALTER TABLE atlas_app.ticket_service ADD COLUMN expiry text ;
ALTER TABLE atlas_app.ticket_service ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_service ADD COLUMN max_verification integer ;
ALTER TABLE atlas_app.ticket_service ADD COLUMN operational_days text[] NOT NULL;
ALTER TABLE atlas_app.ticket_service ADD COLUMN places_id text NOT NULL;
ALTER TABLE atlas_app.ticket_service ADD COLUMN service text NOT NULL;
ALTER TABLE atlas_app.ticket_service ADD COLUMN short_desc text ;
ALTER TABLE atlas_app.ticket_service ADD PRIMARY KEY ( id);

insert into atlas_app.service_people_category (id, name, description, price_per_unit) values ('125378b5-0a86-44ce-afa1-b4d27d712a23', 'Adult', 'Adult', 50);
insert into atlas_app.service_people_category (id, name, description, price_per_unit) values ('225378b5-0a86-44ce-afa1-b4d27d712a23', 'Adult', 'Adult', 20);

insert into atlas_app.service_people_category (id, name, description, price_per_unit) values ('325378b5-0a86-44ce-afa1-b4d27d712a23', 'Adult', 'Adult', 20);
insert into atlas_app.service_people_category (id, name, description, price_per_unit) values ('425378b5-0a86-44ce-afa1-b4d27d712a23', 'Adult', 'Adult', 10);

insert into atlas_app.service_people_category (id, name, description, price_per_unit) values ('525378b5-0a86-44ce-afa1-b4d27d712a23', 'CameraUnit', 'CameraUnit', 250);

insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (null, null, 'all', 'c15378b5-0a86-44ce-afa1-b4d27d712a23', 'all', '{125378b5-0a86-44ce-afa1-b4d27d712a23, 225378b5-0a86-44ce-afa1-b4d27d712a23}');
insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (null, null, 'all', 'c25378b5-0a86-44ce-afa1-b4d27d712a23', 'all', '{325378b5-0a86-44ce-afa1-b4d27d712a23, 425378b5-0a86-44ce-afa1-b4d27d712a23}');
insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (null, null, 'all', 'c35378b5-0a86-44ce-afa1-b4d27d712a23', 'all', '{525378b5-0a86-44ce-afa1-b4d27d712a23}');

insert atlas_app.business_hour (id, btype, category_id) values ('b15378b5-0a86-44ce-afa1-b4d27d712a23', 'Duration 03:30:00 11:30:00', '{125378b5-0a86-44ce-afa1-b4d27d712a23}');
insert atlas_app.business_hour (id, btype, category_id) values ('b2378b5-0a86-44ce-afa1-b4d27d712a23', 'Duration 03:30:00 11:30:00', '{125378b5-0a86-44ce-afa1-b4d27d712a23}');
insert atlas_app.business_hour (id, btype, category_id) values ('b3378b5-0a86-44ce-afa1-b4d27d712a23', 'Duration 05:00:00 11:00:00', '{125378b5-0a86-44ce-afa1-b4d27d712a23}');

update atlas_app.ticket_service set operational_days = '{Monday,Tuesday,Wednesday,Friday,Saturday,Sunday}', allow_future_booking = true, expiry = 'VisitDate 12:00:00';

update atlas_app.ticket_service set business_hour = '{b15378b5-0a86-44ce-afa1-b4d27d712a23}' where id = 'b73378dc-427f-4efa-9b55-8efe7e3352c2';
update atlas_app.ticket_service set business_hour = '{b25378b5-0a86-44ce-afa1-b4d27d712a23}' where id = 'a7eba6ed-99f7-442f-a9d8-00c8b380657b';
update atlas_app.ticket_service set business_hour = '{b35378b5-0a86-44ce-afa1-b4d27d712a23}' where id = 'd8f47b42-50a5-4a97-8dda-e80a3633d7ab';

-- old bookings
insert into atlas_app.ticket_booking_service_category (id, name, amount, booked_seats, ticket_booking_service_id) values(select atlas_app.uuid_generate_v4() as id, 'all' as name, amount, null as booked_seats, id as ticket_booking_service_id from atlas_app.ticket_booking_service);

insert into atlas_app.ticket_booking_people_category (id, name, number_of_units, price_per_unit, ticket_booking_service_category_id) values (select atlas_app.uuid_generate_v4(), attendee_type as name, number_of_units, price_per_unit, tbsc.id as ticket_booking_service_category_id from atlas_app.ticket_booking_service_price_breakup pb left join atlas_app.ticket_booking_service_category as tbsc on tbsc.ticket_booking_service_id = pb.ticket_booking_service_id);
