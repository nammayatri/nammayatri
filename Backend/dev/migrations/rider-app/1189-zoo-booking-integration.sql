CREATE TABLE atlas_app.ticket_place (
    id CHARACTER(36) PRIMARY KEY NOT NULL,
    merchant_operating_city_id CHARACTER(36),
    name text NOT NULL,
    description text,
    lat double precision,
    lon double precision,
    gallery text[],
    open_timings time without time zone,
    close_timings time without time zone
);

CREATE TABLE atlas_app.ticket_service (
    id CHARACTER(36) PRIMARY KEY NOT NULL,
    places_id CHARACTER(36),
    service VARCHAR(50) NOT NULL, -- Entrance | Aquarium | Video Photography
    max_verification INT NOT NULL DEFAULT 1,
    open_timings time without time zone,
    close_timings time without time zone
);

CREATE TABLE atlas_app.ticket_service_price (
    ticket_service_id CHARACTER(36),
    attendee_type VARCHAR(36) NOT NULL, -- Adult | Kid | CameraUnit
    price_per_unit NUMERIC(30, 2) NOT NULL,
    PRIMARY KEY (ticket_service_id, attendee_type)
);

-- User Bookings Table
CREATE TABLE atlas_app.ticket_booking (
    id CHARACTER(36) PRIMARY KEY NOT NULL,
    short_id VARCHAR(36) NOT NULL,
    merchant_operating_city_id CHARACTER(36),
    ticket_place_id CHARACTER(36),
    person_id character (36),
    amount NUMERIC (30, 2) NOT NULL,
    visit_date DATE NOT NULL,
    status VARCHAR(10) NOT NULL, -- pending | failed | booked
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_app.ticket_booking_service (
    id CHARACTER(36) PRIMARY KEY NOT NULL,
    short_id VARCHAR(36) NOT NULL,
    ticket_booking_id CHARACTER(36),
    ticket_service_id CHARACTER(36),

    amount NUMERIC (30, 2) NOT NULL,
    status VARCHAR(10) NOT NULL, -- pending | failed | confirmed | verified
    verification_count INT DEFAULT 0,
    expiry_date TIMESTAMP WITH TIME ZONE,

    merchant_operating_city_id CHARACTER(36),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_app.ticket_booking_service_price_breakup (
    ticket_booking_service_id CHARACTER(36),
    attendee_type VARCHAR(36) NOT NULL,
    number_of_units INT NOT NULL,
    price_per_unit NUMERIC(30, 2) NOT NULL
);

INSERT INTO atlas_app.ticket_place (id, merchant_operating_city_id, name, description, lat, lon, gallery, open_timings, close_timings) VALUES ('1ef78db2-90de-4ed7-a38a-fcbb7ce28135', '7ef7af35-19be-9f28-9330-b346ed992a1d', 'Alipore Zoo',
    'The Zoological Garden, Alipore is Indias oldest formally stated zoological park (as opposed to royal and British menageries) and a big tourist attraction in Kolkata, West Bengal.',
    22.537351383029193,88.33227051149596, '{}', '03:30:00', '12:00:00');

INSERT INTO atlas_app.ticket_service (id, places_id, service, max_verification, open_timings, close_timings) VALUES ('b73378dc-427f-4efa-9b55-8efe7e3352c2', '1ef78db2-90de-4ed7-a38a-fcbb7ce28135', 'Entrance Fee', 2, '03:30:00', '12:00:00');
INSERT INTO atlas_app.ticket_service (id, places_id, service, max_verification, open_timings, close_timings) VALUES ('a7eba6ed-99f7-442f-a9d8-00c8b380657b', '1ef78db2-90de-4ed7-a38a-fcbb7ce28135', 'Aquarium Fee', 2, '05:00:00', '10:50:00');
INSERT INTO atlas_app.ticket_service (id, places_id, service, max_verification, open_timings, close_timings) VALUES ('d8f47b42-50a5-4a97-8dda-e80a3633d7ab', '1ef78db2-90de-4ed7-a38a-fcbb7ce28135', 'Videography Fee', 2, '03:30:00', '12:00:00');

INSERT INTO atlas_app.ticket_service_price (ticket_service_id, attendee_type, price_per_unit) VALUES ('b73378dc-427f-4efa-9b55-8efe7e3352c2', 'Adult', 50.00);
INSERT INTO atlas_app.ticket_service_price (ticket_service_id, attendee_type, price_per_unit) VALUES ('b73378dc-427f-4efa-9b55-8efe7e3352c2', 'Child', 20.00);
INSERT INTO atlas_app.ticket_service_price (ticket_service_id, attendee_type, price_per_unit) VALUES ('a7eba6ed-99f7-442f-a9d8-00c8b380657b', 'Adult', 20.00);
INSERT INTO atlas_app.ticket_service_price (ticket_service_id, attendee_type, price_per_unit) VALUES ('a7eba6ed-99f7-442f-a9d8-00c8b380657b', 'Child', 10.00);
INSERT INTO atlas_app.ticket_service_price (ticket_service_id, attendee_type, price_per_unit) VALUES ('d8f47b42-50a5-4a97-8dda-e80a3633d7ab', 'Camera Unit', 250.00);