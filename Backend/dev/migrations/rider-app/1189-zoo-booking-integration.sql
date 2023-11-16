CREATE TABLE ticket_place (
    id CHARACTER(36) PRIMARY KEY NOT NULL,
    merchant_operating_city_id CHARACTER(36) REFERENCES merchant_operating_city(id),
    name text NOT NULL,
    description text,
    lat double precision,
    lon double precision,
    gallery text[],
    open_timings TIMESTAMP WITH TIME ZONE,
    close_timings TIMESTAMP WITH TIME ZONE
);

CREATE TABLE ticket_service (
    id CHARACTER(36) PRIMARY KEY NOT NULL,
    places_id CHARACTER(36) REFERENCES ticket_place(id),
    service VARCHAR(50) NOT NULL, -- Entrance | Aquarium | Video Photography
    max_verifications INT NOT NULL DEFAULT 1,
    open_timings TIMESTAMP WITH TIME ZONE,
    close_timings TIMESTAMP WITH TIME ZONE
);

CREATE TABLE ticket_service_price (
    ticket_service_id CHARACTER(36) PRIMARY KEY NOT NULL,
    attendee_type VARCHAR(10) NOT NULL, -- Adult | Kid | CameraUnit
    price_per_adult NUMERIC(30, 2) NOT NULL,
    PRIMARY KEY (ticket_service_id, attendee_type)
);

-- User Bookings Table
CREATE TABLE ticket_booking (
    id CHARACTER(36) PRIMARY KEY NOT NULL,
    short_id VARCHAR(36) NOT NULL,
    merchant_operating_city_id CHARACTER(36) REFERENCES merchant_operating_city(id),
    ticket_place_id CHARACTER(36) REFERENCES ticket_place(id),
    person_id character (36),
    amount NUMERIC (30, 2) NOT NULL,
    visit_date DATE NOT NULL,
    status VARCHAR(10) NOT NULL, -- pending | failed | booked
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE ticket_booking_service (
    id CHARACTER(36) PRIMARY KEY NOT NULL,
    short_id VARCHAR(36) NOT NULL,
    ticket_booking_id CHARACTER(36) REFERENCES ticket_booking(id),
    ticket_service_id CHARACTER(36) REFERENCES ticket_service(id),

    amount NUMERIC (30, 2) NOT NULL,
    status VARCHAR(10) NOT NULL, -- pending | failed | confirmed | verified
    verification_count INT DEFAULT 0,
    expiry_date TIMESTAMP WITH TIME ZONE DEFAULT,

    merchant_operating_city_id CHARACTER(36) REFERENCES merchant_operating_city(id),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE ticket_booking_service_price_breakup (
    ticket_booking_service_id CHARACTER(36) REFERENCES ticket_booking_service(id),
    attendee_type VARCHAR(10) NOT NULL,
    number_of_units INT NOT NULL,
    price_per_unit NUMERIC(30, 2) NOT NULL
);