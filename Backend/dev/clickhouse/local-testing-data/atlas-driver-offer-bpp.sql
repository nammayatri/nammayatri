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
        collected_by,
        updated_at
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
        '8b278f98-f9fb-4be6-a19f-175ffe2ac1fb',
        '2024-01-22 07:02:42'
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
        collected_by,
        updated_at
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
        '8b278f98-f9fb-4be6-a19f-175ffe2ac1fb',
        '2024-01-22 07:02:42'
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
        collected_by,
        updated_at
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
        '8b278f98-f9fb-4be6-a19f-175ffe2ac1fb',
        '2024-01-22 07:02:42'
    );

INSERT INTO
    atlas_driver_offer_bpp.ride_details (
        id,
        vehicle_number,
        fleet_owner_id,
        created_at
    )
VALUES
    (
        '1119bb0b-b012-4d1a-abed-32555212e111',
        'KA01HA0001',
        '3680f4b5-dce4-4d03-aa8c-5405690e87bd',
        '2024-01-22 07:02:42'
    );

INSERT INTO
    atlas_driver_offer_bpp.ride_details (
        id,
        vehicle_number,
        fleet_owner_id,
        created_at
    )
VALUES
    (
        '2229bb0b-b012-4d1a-abed-32555212e222',
        'KA01HA0001',
        '3680f4b5-dce4-4d03-aa8c-5405690e87bd',
        '2024-01-22 07:02:42'
    );

INSERT INTO
    atlas_driver_offer_bpp.ride_details (
        id,
        vehicle_number,
        fleet_owner_id,
        created_at
    )
VALUES
    (
        '3339bb0b-b012-4d1a-abed-32555212e333',
        'KA01HA0001',
        '3680f4b5-dce4-4d03-aa8c-5405690e87bd',
        '2024-01-22 07:02:42'
    );

INSERT INTO
    atlas_driver_offer_bpp.ride (
        id,
        status,
        fare,
        driver_id,
        chargeable_distance,
        created_at,
        updated_at
    )
VALUES
    (
        '3339bb0b-b012-4d1a-abed-32555212e333',
        'COMPLETED',
        '352',
        'favorit-bike-00000000000000000000000',
        '352',
        '2024-01-22 07:02:42',
        '2024-01-22 07:02:42'
    );

INSERT INTO
    atlas_driver_offer_bpp.ride (
        id,
        status,
        fare,
        driver_id,
        chargeable_distance,
        created_at,
        updated_at
    )
VALUES
    (
        '2229bb0b-b012-4d1a-abed-32555212e222',
        'COMPLETED',
        '247',
        'favorit-bike-00000000000000000000000',
        '248',
        '2024-01-22 07:02:42',
        '2024-01-22 07:02:42'
    );

INSERT INTO
    atlas_driver_offer_bpp.ride (
        id,
        status,
        fare,
        driver_id,
        chargeable_distance,
        created_at,
        updated_at
    )
VALUES
    (
        'favorit-auto1-0000000000000000000001',
        'CANCELLED',
        '112',
        'favorit-auto1-0000000000000000000000',
        '122',
        '2024-01-22 07:02:42',
        '2024-01-22 07:02:42'
    );

INSERT INTO
    atlas_driver_offer_bpp.ride (
        id,
        status,
        fare,
        driver_id,
        chargeable_distance,
        created_at,
        updated_at
    )
VALUES
    (
        'favorit-auto1-0000000000000000000002',
        'COMPLETED',
        '362',
        'favorit-auto1-0000000000000000000000',
        '372',
        '2024-01-22 07:02:42',
        '2024-01-22 07:02:42'
    );

INSERT INTO
    atlas_driver_offer_bpp.ride (
        id,
        status,
        fare,
        driver_id,
        chargeable_distance,
        created_at,
        updated_at
    )
VALUES
    (
        'favorit-auto1-0000000000000000000003',
        'COMPLETED',
        '257',
        'favorit-auto1-0000000000000000000000',
        '268',
        '2024-01-22 07:02:42',
        '2024-01-22 07:02:42'
    );

INSERT INTO
    atlas_driver_offer_bpp.ride (
        id,
        status,
        fare,
        driver_id,
        chargeable_distance,
        created_at,
        updated_at
    )
VALUES
    (
        'favorit-auto1-0000000000000000000004',
        'CANCELLED',
        '112',
        'favorit-auto1-0000000000000000000000',
        '122',
        '2024-01-22 07:02:42',
        '2024-01-22 07:02:42'
    );