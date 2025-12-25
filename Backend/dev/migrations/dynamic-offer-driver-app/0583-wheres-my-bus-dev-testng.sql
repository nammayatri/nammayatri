--------------------------------------------------------------------------------------------------------
--------------------------- LOCAL TESTING, DO NOT RUN IN MASTER OR PROD --------------------------------
--------------------------------------------------------------------------------------------------------
INSERT INTO
  atlas_driver_offer_bpp.route (
    code,
    end_lat,
    end_lon,
    id,
    long_name,
    short_name,
    start_lat,
    start_lon,
    polyline,
    time_bounds,
    vehicle_type,
    merchant_id,
    merchant_operating_city_id,
    created_at,
    updated_at,
    round_route_code
  )
VALUES
  (
    'Route:1:S1',
    22.64223,
    88.43121,
    md5(random() :: text || clock_timestamp() :: text) :: uuid,
    'ECO T PARK-AIRPORT GATE NO1',
    'SHUTTLEU',
    22.5945,
    88.4728,
    'wegnA__whNy@{@SKQGUC_A@@MCUAO?wB@_F?{Q@{A?aTAcCCaDMWGKMEICgDn@g@Na@\\ALQ^OHYAq@W_@?YDa@JWDQAc@@yARmAN}@NeFx@uATAWsEr@yDr@wDj@sF~@gBXcDf@BPz@Mx@KuBXkG`AoEr@oDh@kGdA_Dd@_DZoF\\aABUIk@P_BNo@Ze@`@Yf@Wx@AFIj@BxB@dD@zHBbD?hBIjBU|CCR[C{CWwAIqHI}A@_BDqHByBDoCAkA@eCIcCMwCSoBS_Ei@}@McAUaCm@iIeBqLqCeB]?@wIsBkGwAeEkAYSQWOa@WsAQcAO]UQUKuCQgDEO??KBm@XwDZ}DLyAN}@d@_DR}@JW}@g@w@m@qB{AwDeCwBoA{DmCuEeDgAq@yDoCk@]ObAEt@Ax@oEQaBE?_@IkJqH{EqE}C?Ae@YwB}@uCwAeD{AiB{@oAg@mASs@I{AI{E[{BUMAUU{CaBy@g@iBeAaAw@}BkAiA_@eBg@mCk@wAO_@E}@[WMGCq@]aCoBoB}AGKIA{AI}AIk@Gu@Iy@Wo@MmEm@yE[{@Mm@Mk@WA@KEwBy@w@a@c@KUMwCUcAKo@C{@DQBeCdAKD]AQCOIsCyDqBkCc@e@_@S{AQoFg@_Ee@O?MF_@Zu@eAk@aAIEWKk@[WGy@e@yA{@]OsB_AA]LuABwABiBDoA{@wAUSa@O{Am@e@[YUcAuAwDaF{@WiC_A}@o@eAkA_GuGu@FkFP_A@aNRWFOLKXS~@Sl@_@\\OFYFa@?}@KsAOs@EY?aGNcADO?[QGCEKIg@AOEo@?M^QLCGi@IaAq@sJIkBc@}JCcA@wAa@UsAw@}C_BqCcBeA}@W_@kAqBWm@Ok@}J}A}Dm@?tAB|B@v@',
    'Unbounded',
    'BUS',
    'favorit0-0000-0000-0000-00000favorit',
    'favorit0-0000-0000-0000-00000000city',
    now(),
    now(),
    NULL
  );


INSERT INTO
  atlas_driver_offer_bpp.route_trip_stop_mapping (
    provider_code,
    trip_code,
    route_code,
    stop_code,
    stop_name,
    stop_lat,
    stop_lon,
    stop_sequence_num,
    trip_sequence_num,
    vehicle_type,
    merchant_id,
    merchant_operating_city_id,
    created_at,
    updated_at,
    scheduled_day,
    enabled,
    scheduled_departure,
    scheduled_arrival
  )
SELECT
    '1',
    'Trip:1:' || t.days_of_week,
    'Route:1:S' || s.seq_num,
    'Stop:1:18465',
    'ECO T PARK',
    22.5945,
    88.4728,
    s.seq_num,
    1,
    'BUS',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP,
    t.days_of_week,
    TRUE,
    '00:00:00'::time,
    '23:59:59'::time
FROM
    UNNEST(ARRAY['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']) AS t(days_of_week)
    CROSS JOIN GENERATE_SERIES(1, 10) AS s(seq_num);

-- favorit-fleet-owner-0000000000000000 (fleet owner id) create fleet_driver_association and vehicle_route_mapping

INSERT INTO atlas_driver_offer_bpp.vehicle_route_mapping (
    fleet_owner_id,
    vehicle_number_hash,
    vehicle_number_encrypted,
    route_code,
    merchant_id,
    merchant_operating_city_id,
    blocked
)
VALUES
    (
        'favorit-fleet-owner-0000000000000000',
        '\xb75e10098121f04df118f6e5a1ab0bf536f9dd5211b3a91543619dc336d59744',
        '0.1.0|0|xgLuhzcv2xpOx6Ig1ZiJoxqgsWW+vB6E3cF6dSqmr3P2TnTDUHPWEtGKNfQ3KnDCzqLn5pvUd1DL4jxg', -- WB1234567
        'Route:1:S1',
        'favorit0-0000-0000-0000-00000favorit',
        'favorit0-0000-0000-0000-00000000city',
        false
    );

INSERT INTO atlas_driver_offer_bpp.fleet_driver_association (
    id,
    driver_id,
    is_active,
    fleet_owner_id,
    associated_on,
    associated_till,
    created_at,
    updated_at
)
VALUES
    (
        '9a9cf313-8aaf-4546-a4b7-82221a6b47f4',
        'favorit-fleet-owner-0000000000000000',
        true,
        'favorit-fleet-owner-0000000000000000',
        NULL,
        NULL,
        now(),
        now()
    );

INSERT INTO atlas_driver_offer_bpp.vehicle_registration_certificate (
    id,
    fleet_owner_id,
    certificate_number_hash,
    certificate_number_encrypted,
    fitness_expiry,
    created_at,
    verification_status,
    updated_at,
    failed_rules,
    document_image_id,
    vehicle_variant,
    merchant_id,
    merchant_operating_city_id
)
VALUES
    (
        md5(random()::text || clock_timestamp()::text)::uuid,
        'favorit-fleet-owner-0000000000000000',
        '\xb75e10098121f04df118f6e5a1ab0bf536f9dd5211b3a91543619dc336d59744',
        '0.1.0|0|xgLuhzcv2xpOx6Ig1ZiJoxqgsWW+vB6E3cF6dSqmr3P2TnTDUHPWEtGKNfQ3KnDCzqLn5pvUd1DL4jxg',
        '2027-03-30 00:00:00+00',
        now(),
        'VALID',
        now(),
        '{}',
        '6bf6fac7-76c0-4f7b-9718-8c33844e4e03',
        'BUS_AC',
        'favorit0-0000-0000-0000-00000favorit',
        'favorit0-0000-0000-0000-00000000city'
    );

INSERT INTO atlas_driver_offer_bpp.fleet_route_association (
  fleet_owner_id,
  id,
  merchant_id,
  merchant_operating_city_id,
  route_code,
  created_at,
  updated_at
) VALUES
( 'favorit-fleet-owner-0000000000000000',
  md5(random() :: text || clock_timestamp() :: text) :: uuid,
  'favorit0-0000-0000-0000-00000favorit',
  'favorit0-0000-0000-0000-00000000city',
  'Route:1:S1',
  now(),
  now()
);

INSERT INTO atlas_driver_offer_bpp.fleet_config (
    allow_ending_mid_route,
    end_ride_distance_threshold,
    fleet_owner_id,
    ride_end_approval,
    allow_start_ride_from_qr,
    allow_automatic_round_trip_assignment,
    merchant_id,
    merchant_operating_city_id
)
SELECT
    true,
    500.0,
    'favorit-fleet-owner-0000000000000000',
    true,
    false,
    false,
    m.merchant_id,
    m.id
FROM atlas_driver_offer_bpp.merchant_operating_city as m WHERE city = 'Kochi' and merchant_short_id = 'NAMMA_YATRI_PARTNER';

INSERT INTO atlas_driver_offer_bpp.document_verification_config (
    check_expiry,
    check_extraction,
    dependency_document_type,
    description,
    disable_warning,
    document_type,
    is_disabled,
    is_hidden,
    is_mandatory,
    max_retry_count,
    merchant_id,
    merchant_operating_city_id,
    rc_number_prefix_list,
    supported_vehicle_classes_json,
    title,
    vehicle_category,
    vehicle_class_check_type,
    created_at,
    updated_at,
    is_default_enabled_on_manual_verification,
    is_image_validation_required
)
SELECT
    check_expiry,
    check_extraction,
    dependency_document_type,
    description,
    disable_warning,
    document_type,
    is_disabled,
    is_hidden,
    is_mandatory,
    max_retry_count,
    merchant_id,
    (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kochi' and merchant_short_id = 'NAMMA_YATRI_PARTNER') as merchant_operating_city_id,
    '{WB}',
    supported_vehicle_classes_json,
    title,
    'BUS' AS vehicle_category,
    vehicle_class_check_type,
    CURRENT_TIMESTAMP as created_at,
    CURRENT_TIMESTAMP as updated_at,
    is_default_enabled_on_manual_verification,
    is_image_validation_required
FROM
    atlas_driver_offer_bpp.document_verification_config
WHERE
    vehicle_category = 'CAR'
    AND merchant_operating_city_id = (select merchant_operating_city_id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kochi' and merchant_short_id = 'NAMMA_YATRI_PARTNER')
    AND document_type IN ('ProfilePhoto', 'DriverLicense', 'Permissions', 'AadhaarCard')
ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.document_verification_config (
    check_expiry,
    check_extraction,
    dependency_document_type,
    description,
    disable_warning,
    document_type,
    is_disabled,
    is_hidden,
    is_mandatory,
    max_retry_count,
    merchant_id,
    merchant_operating_city_id,
    rc_number_prefix_list,
    supported_vehicle_classes_json,
    title,
    vehicle_category,
    vehicle_class_check_type,
    created_at,
    updated_at,
    is_default_enabled_on_manual_verification,
    is_image_validation_required
)
SELECT
    false as check_expiry,
    false as check_extraction,
    dependency_document_type,
    description,
    disable_warning,
    document_type,
    is_disabled,
    true as is_hidden,
    false as is_mandatory,
    max_retry_count,
    (select merchant_id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kochi' and merchant_short_id = 'NAMMA_YATRI_PARTNER') as merchant_id,
    (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kochi' and merchant_short_id = 'NAMMA_YATRI_PARTNER') as merchant_operating_city_id,
    '{WB}',
    supported_vehicle_classes_json,
    title,
    'BUS' AS vehicle_category,
    vehicle_class_check_type,
    CURRENT_TIMESTAMP as created_at,
    CURRENT_TIMESTAMP as updated_at,
    is_default_enabled_on_manual_verification,
    is_image_validation_required
FROM
    atlas_driver_offer_bpp.document_verification_config
WHERE
    vehicle_category = 'CAR'
    AND merchant_operating_city_id = (select merchant_operating_city_id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kochi' and merchant_short_id = 'NAMMA_YATRI_PARTNER')
    AND document_type IN ('VehicleRegistrationCertificate')
ON CONFLICT DO NOTHING;

UPDATE atlas_driver_offer_bpp.document_verification_config
SET supported_vehicle_classes_json = '[{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "3wt_cab", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null},{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "hmv", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null},{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "hpv", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null},{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "mpv", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null},{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "htv", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null},{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "lmv", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null},{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "trans", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null},{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "mgv", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null},{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "hgmv", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null},{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "lgv", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null},{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "hgv", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null},{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "hgv-tr", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null},{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "hgv-trans", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null},{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "hgvtr", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null},{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "hgvtrans", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null},{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "lmv-tr", "vehicleModel": null, "reviewRequired": false, "vehicleVariant": "BUS_NON_AC", "vehicleCapacity": null, "manufacturerModel": null}]'
WHERE document_type = 'VehicleRegistrationCertificate' AND vehicle_category = 'BUS' AND merchant_operating_city_id = (select merchant_operating_city_id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kochi' and merchant_short_id = 'NAMMA_YATRI_PARTNER');

UPDATE atlas_driver_offer_bpp.document_verification_config
SET supported_vehicle_classes_json = json_build_array('HMV', 'HPV', 'MPV', 'HTV', 'LMV', 'TRANS', 'MGV', 'HGMV', 'LGV', 'HGV', 'HGV-TR', 'HGV-TRANS', 'HGVTR', 'HGVTRANS', 'LMV-TR', '3WT_CAB')
WHERE document_type = 'DriverLicense' AND vehicle_category = 'BUS' AND merchant_operating_city_id = (select merchant_operating_city_id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kochi' and merchant_short_id = 'NAMMA_YATRI_PARTNER');