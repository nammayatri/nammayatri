--------------------------------------------------------------------------------------------------------
--------------------------- LOCAL TESTING, DO NOT RUN IN MASTER OR PROD --------------------------------
--------------------------------------------------------------------------------------------------------

insert into atlas_app.ticket_place (
    id,
    merchant_operating_city_id,
    name,
    description,
    lat,
    lon,
    gallery,
    open_timings,
    close_timings,
    short_desc,
    icon_url,
    map_image_url,
    terms_and_conditions,
    place_type,
    status,
    allow_same_day_booking,
    priority,
    is_recurring,
    is_closed,
    assign_ticket_to_bpp
) values (
    'k1l2m3n4-o5p6-7890-1234-567890abcdef',
    'namma-yatri-0-0000-0000-00000000city',
    'Chilika Boating - Satapada',
    'Where the tranquil waters of Chilika Lake merge with the boundless azure of the Bay of Bengal, a natural sanctuary awaits just 50 kilometres from Puri. This serene destination offers boat rides where you can witness the majestic Irrawaddy dolphins in their natural habitat. Life jackets are provided and must be worn. One boat holds up to 4 persons. Experience the perfect blend of adventure and tranquility as you navigate through the largest brackish water lagoon in Asia.',
    19.6683,
    85.4489,
    '{"https://assets.moving.tech/beckn/jatrisaathi/user/images/ny_ic_chilika_boating.png"}',
    '10:00:00',
    '18:00:00',
    'Book boat rides at Chilika Lake - Satapada',
    'https://assets.moving.tech/beckn/jatrisaathi/user/images/ny_ic_chilika_boating_icon.png',
    'https://assets.moving.tech/beckn/jatrisaathi/user/images/ny_ic_chilika_boating_map.png',
    '{"Life jackets are provided and must be worn", "One boat holds up to 4 persons", "Cancellation of tickets is not applicable", "Children below 5 years are not allowed", "Tickets necessary from 8 years and above"}',
    'Boating',
    'Active',
    true,
    0,
    true,
    false,
    false
);

-- 2. Insert ticket service
insert into atlas_app.ticket_service(
    id,
    places_id,
    service,
    max_verification,
    allow_future_booking,
    business_hours,
    expiry,
    operational_days,
    short_desc
) values (
    'l2m3n4o5-p6q7-8901-2345-67890abcdef0',
    'k1l2m3n4-o5p6-7890-1234-567890abcdef',
    'Satapada Dolphin Chilika',
    1,
    true,
    '{m3n4o5p6-q7r8-9012-3456-7890abcdef01}',
    'VisitDate 18:00:00',
    '{Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday}',
    'Boat ride service at Satapada with dolphin watching'
);

-- 3. Insert business hours
insert into atlas_app.business_hour (
    btype,
    category_id,
    id
) values (
    'Duration 10:00:00 18:00:00',
    '{n4o5p6q7-r8s9-0123-4567-890abcdef012}',
    'm3n4o5p6-q7r8-9012-3456-7890abcdef01'
);

-- 4. Insert service category
insert into atlas_app.service_category (
    allowed_seats,
    available_seats,
    description,
    id,
    name,
    people_category
) values (
    null,
    null,
    'Boat ride service for all durations and times',
    'n4o5p6q7-r8s9-0123-4567-890abcdef012',
    'Boating',
    '{o5p6q7r8-s9t0-1234-5678-90abcdef0123}'
);

-- 5. Insert service people category (base price)
insert into atlas_app.service_people_category (
    description,
    id,
    name,
    price_per_unit,
    time_bounds
) values (
    'Adult',
    'o5p6q7r8-s9t0-1234-5678-90abcdef0123',
    'Adult',
    800,
    'Unbounded'
);

-- 6. Weekend and holiday pricing
insert into atlas_app.service_people_category (
    description,
    id,
    name,
    price_per_unit,
    time_bounds
) values (
    'Adult',
    'f6a7b8c9-d0e1-2345-6789-0abcdef12345',
    'Adult',
    1000,
    'BoundedPeaks {monday = [], tuesday = [], wednesday = [], thursday = [], friday = [], saturday = [(00:00:00,23:59:59)], sunday = [(00:00:00,23:59:59)]}'
);

-- 7. Insert ticket sub-places for different boat ride durations
-- 0.5 Hour Boat Ride
insert into atlas_app.ticket_sub_place (
    id,
    ticket_place_id,
    name,
    description,
    sub_place_type,
    is_active,
    rules
) values (
    'g7b8c9d0-e1f2-3456-7890-1bcdef23456',
    'k1l2m3n4-o5p6-7890-1234-567890abcdef',
    '0.5 Hour Boat Ride',
    'Quick 30-minute boat ride experience at Chilika Lake. Perfect for a brief introduction to the lake and dolphin watching. Life jackets are provided and must be worn. One boat holds up to 4 persons.',
    'Dock',
    true,
    '{"rules": ["Life jackets are provided and must be worn", "One boat holds up to 4 persons", "Duration: 30 minutes", "Cancellation of tickets is not applicable"]}'
);

-- 1 Hour Boat Ride
insert into atlas_app.ticket_sub_place (
    id,
    ticket_place_id,
    name,
    description,
    sub_place_type,
    is_active,
    rules
) values (
    'h8c9d0e1-f2a3-4567-8901-2cdef34567',
    'k1l2m3n4-o5p6-7890-1234-567890abcdef',
    '1 Hour Boat Ride',
    'One-hour comprehensive boat ride experience at Chilika Lake. Extended time for dolphin watching and exploring the scenic beauty of the largest brackish water lagoon in Asia. Life jackets are provided and must be worn. One boat holds up to 4 persons.',
    'Dock',
    true,
    '{"rules": ["Life jackets are provided and must be worn", "One boat holds up to 4 persons", "Duration: 1 hour", "Cancellation of tickets is not applicable"]}'
);

-- 2 Hour Boat Ride
insert into atlas_app.ticket_sub_place (
    id,
    ticket_place_id,
    name,
    description,
    sub_place_type,
    is_active,
    rules
) values (
    'i9d0e1f2-a3b4-5678-9012-3def45678',
    'k1l2m3n4-o5p6-7890-1234-567890abcdef',
    '2 Hour Boat Ride',
    'Two-hour extended boat ride adventure at Chilika Lake. Ample time for dolphin watching, bird watching, and exploring the diverse ecosystem of the lake. Life jackets are provided and must be worn. One boat holds up to 4 persons.',
    'Dock',
    true,
    '{"rules": ["Life jackets are provided and must be worn", "One boat holds up to 4 persons", "Duration: 2 hours", "Cancellation of tickets is not applicable"]}'
);

-- 4 Hour Boat Ride
insert into atlas_app.ticket_sub_place (
    id,
    ticket_place_id,
    name,
    description,
    sub_place_type,
    is_active,
    rules
) values (
    'j0e1f2a3-b4c5-6789-0123-4ef56789',
    'k1l2m3n4-o5p6-7890-1234-567890abcdef',
    '4 Hour Boat Ride',
    'Four-hour comprehensive boat ride experience at Chilika Lake. Complete exploration of the lake including dolphin watching, bird watching, fishing villages, and the scenic beauty of the largest brackish water lagoon in Asia. Life jackets are provided and must be worn. One boat holds up to 4 persons.',
    'Dock',
    true,
    '{"rules": ["Life jackets are provided and must be worn", "One boat holds up to 4 persons", "Duration: 4 hours", "Cancellation of tickets is not applicable"]}'
);

-- 8. Add special occasions for holidays (example for a few holidays)
-- Republic Day
insert into atlas_app.special_occasion (
    business_hours,
    date,
    day_of_week,
    description,
    entity_id,
    id,
    special_day_type
) values (
    '{m3n4o5p6-q7r8-9012-3456-7890abcdef01}',
    '2025-01-26',
    null,
    null,
    'l2m3n4o5-p6q7-8901-2345-67890abcdef0',
    md5(
        random() :: TEXT || clock_timestamp() :: TEXT || random() :: TEXT || random() :: TEXT || random() :: TEXT || random() :: TEXT
    ) :: UUID,
    'Open'
);

-- Independence Day
insert into atlas_app.special_occasion (
    business_hours,
    date,
    day_of_week,
    description,
    entity_id,
    id,
    special_day_type
) values (
    '{m3n4o5p6-q7r8-9012-3456-7890abcdef01}',
    '2025-08-15',
    null,
    null,
    'l2m3n4o5-p6q7-8901-2345-67890abcdef0',
    md5(
        random() :: TEXT || clock_timestamp() :: TEXT || random() :: TEXT || random() :: TEXT || random() :: TEXT || random() :: TEXT
    ) :: UUID,
    'Open'
);

-- Gandhi Jayanti
insert into atlas_app.special_occasion (
    business_hours,
    date,
    day_of_week,
    description,
    entity_id,
    id,
    special_day_type
) values (
    '{m3n4o5p6-q7r8-9012-3456-7890abcdef01}',
    '2025-10-02',
    null,
    null,
    'l2m3n4o5-p6q7-8901-2345-67890abcdef0',
    md5(
        random() :: TEXT || clock_timestamp() :: TEXT || random() :: TEXT || random() :: TEXT || random() :: TEXT || random() :: TEXT
    ) :: UUID,
    'Open'
);

-- Christmas
insert into atlas_app.special_occasion (
    business_hours,
    date,
    day_of_week,
    description,
    entity_id,
    id,
    special_day_type
) values (
    '{m3n4o5p6-q7r8-9012-3456-7890abcdef01}',
    '2025-12-25',
    null,
    null,
    'l2m3n4o5-p6q7-8901-2345-67890abcdef0',
    md5(
        random() :: TEXT || clock_timestamp() :: TEXT || random() :: TEXT || random() :: TEXT || random() :: TEXT || random() :: TEXT
    ) :: UUID,
    'Open'
);

-- New Year's Day
insert into atlas_app.special_occasion (
    business_hours,
    date,
    day_of_week,
    description,
    entity_id,
    id,
    special_day_type
) values (
    '{m3n4o5p6-q7r8-9012-3456-7890abcdef01}',
    '2026-01-01',
    null,
    null,
    'l2m3n4o5-p6q7-8901-2345-67890abcdef0',
    md5(
        random() :: TEXT || clock_timestamp() :: TEXT || random() :: TEXT || random() :: TEXT || random() :: TEXT || random() :: TEXT
    ) :: UUID,
    'Open'
);
