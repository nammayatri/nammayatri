INSERT INTO
    atlas_driver_offer_bpp.driver_fee (
        id,
        merchant_id,
        driver_id,
        status,
        num_rides,
        platform_fee,
        cgst,
        sgst,
        govt_charges,
        special_zone_amount,
        collected_at,
        collected_by
    )
VALUES
    (
        '5289bb0b-b012-4d1a-abed-32555212ea04',
        'favorit0-0000-0000-0000-00000favorit',
        'favorit-auto1-0000000000000000000000',
        'COLLECTED_CASH',
        '3',
        '3.0',
        '2.0',
        '1.5',
        6,
        '2.0',
        '2024-01-22 07:02:42',
        '8b278f98-f9fb-4be6-a19f-175ffe2ac1fb'
    );
INSERT INTO
    atlas_driver_offer_bpp.driver_fee (
        id,
        merchant_id,
        driver_id,
        status,
        num_rides,
        platform_fee,
        cgst,
        sgst,
        govt_charges,
        special_zone_amount,
        collected_at,
        collected_by
    )
VALUES
    (
        '5289bb0b-b012-4d1a-abed-32555212ea03',
        'favorit0-0000-0000-0000-00000favorit',
        'favorit-auto1-0000000000000000000000',
        'PAYMENT_PENDING',
        '5',
        '4.0',
        '3.0',
        '2.5',
        7,
        '3.0',
        '2024-01-22 07:02:42',
        '8b278f98-f9fb-4be6-a19f-175ffe2ac1fb'
    );
INSERT INTO
    atlas_driver_offer_bpp.driver_fee (
        id,
        merchant_id,
        driver_id,
        status,
        num_rides,
        platform_fee,
        cgst,
        sgst,
        govt_charges,
        special_zone_amount,
        collected_at,
        collected_by
    )
VALUES
    (
        '5289bb0b-b012-4d1a-abed-32555212ea02',
        'favorit0-0000-0000-0000-00000favorit',
        'favorit-auto1-0000000000000000000000',
        'CLEARED',
        '6',
        '5.0',
        '4.0',
        '3.5',
        8,
        '5.0',
        '2024-01-22 07:02:42',
        '8b278f98-f9fb-4be6-a19f-175ffe2ac1fb'
    );

INSERT INTO
    atlas_driver_offer_bpp.ride_details (
        id,
        vehicle_number,
        fleet_owner_id
    )
VALUES
    (
        '1119bb0b-b012-4d1a-abed-32555212e111',
        'KA01HA0001',
        '3680f4b5-dce4-4d03-aa8c-5405690e87bd'
    );

INSERT INTO
    atlas_driver_offer_bpp.ride_details (
        id,
        vehicle_number,
        fleet_owner_id
    )
VALUES
    (
        '2229bb0b-b012-4d1a-abed-32555212e222',
        'KA01HA0001',
        '3680f4b5-dce4-4d03-aa8c-5405690e87bd'
    );

INSERT INTO
    atlas_driver_offer_bpp.ride_details (
        id,
        vehicle_number,
        fleet_owner_id
    )
VALUES
    (
        '3339bb0b-b012-4d1a-abed-32555212e333',
        'KA01HA0001',
        '3680f4b5-dce4-4d03-aa8c-5405690e87bd'
    );

INSERT INTO
    atlas_driver_offer_bpp.ride (
        id,
        status,
        fare,
        driver_id
    )
VALUES
    (
        '3339bb0b-b012-4d1a-abed-32555212e333',
        'COMPLETED',
        '352',
        'abcdbb0b-b012-4d1a-abed-32555212e213'
    );

INSERT INTO
    atlas_driver_offer_bpp.ride (
        id,
        status,
        fare,
        driver_id
    )
VALUES
    (
        '2229bb0b-b012-4d1a-abed-32555212e222',
        'COMPLETED',
        '247',
        'abcdbb0b-b012-4d1a-abed-32555212e213'
    );

INSERT INTO
    atlas_driver_offer_bpp.ride (
        id,
        status,
        fare,
        driver_id
    )
VALUES
    (
        '1119bb0b-b012-4d1a-abed-32555212e111',
        'CANCELLED',
        '102',
        'abcdbb0b-b012-4d1a-abed-32555212e213'
    );


INSERT INTO atlas_driver_offer_bpp.govt_data (id, merchant_operating_city_id, owner_serial_number, registration_number, permit_validity_from, permit_validity_upto, manufacturer, body_type, number_of_cylinder, fuel_type, seating_capacity, from_date, to_date, created_at)
VALUES
('1', 'b7e43ce1-ae00-082a-5fad-7fc81b86646a', 'SN1', 'TS12345', '2024-01-01', '2027-01-01', 'Manufacturer1', 'Monocoque', 4, 'Petrol', 4, '2021-01-01', '2021-12-31', now()),
('2', 'b7e43ce1-ae00-082a-5fad-7fc81b86646a', 'SN2', 'TS23456', '2024-02-01', '2028-02-01', 'Manufacturer2', 'Monocoque', 6, 'Diesel', 4, '2021-02-01', '2021-12-31', now()),
('3', 'b7e43ce1-ae00-082a-5fad-7fc81b86646a', 'SN3', 'TS14355', '2024-03-01', '2029-03-01', 'Manufacturer3', 'Hackney', 4, 'Electric', 4, '2021-03-01', '2021-12-31', now()),
('4', 'b7e43ce1-ae00-082a-5fad-7fc81b86646a', 'SN4', 'RN4', '2023-04-01', '2028-04-01', 'Manufacturer4', 'Type4', 8, 'Hybrid', 6, '2021-04-01', '2021-12-31', now()),
('5', 'b7e43ce1-ae00-082a-5fad-7fc81b86646a', 'SN5', 'RN5', '2023-05-01', '2029-05-01', 'Manufacturer5', 'Type5', 6, 'Gas', 4, '2021-05-01', '2021-12-31', now());