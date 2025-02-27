---------------------------------------------------------------------------------------------------
--------------------------------- ALL QUERIES ARE ONLY FOR LOCAL ----------------------------------
---------------------------------------------------------------------------------------------------
INSERT INTO
  atlas_app.merchant_service_config (
    merchant_id,
    service_name,
    config_json,
    merchant_operating_city_id
  )
VALUES
  (
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'BusPayment_Juspay',
    '{"apiKey": "0.1.0|0|eWbvOvEN9fe6jIqS4eLDl5dHJk2+HnEdB1EYZH5sJF9jf51Y4VTzITBdSE/NceSq4FKYl3fZ2u03", "returnUrl": "https://api.juspay.in/end", "url": "https://api.juspay.in/", "merchantId": "nammayatri", "username": "nammayatri", "password": "0.1.0|0|eWbvOvEN9fe6jIqS4eLDl5dHJk2+HnEdB1EYZH5sJF9jf51Y4VTzITBdSE/NceSq4FKYl3fZ2u03"}',
    'namma-yatri-0-0000-0000-00000000city'
  );

-- ONLY FOR LOCAL
INSERT INTO
  atlas_app.beckn_config(
    bap_ifsc,
    buyer_finder_fee,
    collected_by,
    confirm_buffer_ttl_sec,
    confirm_ttl_sec,
    domain,
    gateway_url,
    id,
    init_ttl_sec,
    payment_params_json,
    registry_url,
    search_ttl_sec,
    select_ttl_sec,
    settlement_type,
    settlement_window,
    static_terms_url,
    subscriber_id,
    subscriber_url,
    vehicle_category,
    merchant_id,
    merchant_operating_city_id,
    created_at,
    updated_at,
    track_ttl_sec,
    status_ttl_sec,
    rating_ttl_sec,
    cancel_ttl_sec,
    unique_key_id
  )
VALUES
  (
    null,
    null,
    'BAP',
    10,
    120,
    'FRFS',
    'http://localhost:8015/v1',
    'dd22a05d-29a3-42c8-9c8d-2de340f93333',
    120,
    '{"bankAccNumber": "xyz@upi","bankCode": "xyz"}',
    'http://localhost:8020',
    120,
    120,
    null,
    null,
    null,
    'localhost:8013/beckn/frfs/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'http://localhost:8013/beckn/frfs/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'BUS',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now(),
    30,
    30,
    120,
    30,
    'localhost/beckn/frfs/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52'
  );

-- ONLY FOR LOCAL
INSERT INTO
  atlas_app.integrated_bpp_config(
    id,
    domain,
    merchant_id,
    merchant_operating_city_id,
    vehicle_category,
    platform_type,
    config_json,
    created_at,
    updated_at
  )
VALUES
  (
    'dabe23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'FRFS',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'BUS',
    'APPLICATION',
    '{"tag": "EBIX", "contents": {"agentId": "5185", "username": "ATKT", "password": "0.1.0|0|QXWuKw1yei1Pn8G/PcGhwzaRtAWh47owbNnEYNZc0Gbo1qnDmJtwONUzNoPImof7Mw48OQrwdgZsG1I=", "networkHostUrl": "https://app.pmpml.in/CstcTestApi"}}',
    now(),
    now()
  );

-- ONLY FOR LOCAL
INSERT INTO
  atlas_app.frfs_vehicle_service_tier (
    id,
    merchant_id,
    merchant_operating_city_id,
    type,
    provider_code,
    short_name,
    long_name,
    description
  )
VALUES
  (
    '59e722df-feb0-4a86-bd5b-015cf118f400',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'AC',
    '6',
    'ALA',
    'ASHOK LEYLAND AC',
    'অশোক লেইল্যান্ড এসি'
  ),
  (
    '59e722df-feb0-4a86-bd5b-015cf118f401',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'NON_AC',
    '9',
    'MDA',
    'MIDI AC',
    'মিডিয়াম এসি'
  ),
  (
    '59e722df-feb0-4a86-bd5b-015cf118f402',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'AC',
    '3',
    'VOL',
    'VOLVO AC',
    'ভলভো এসি'
  ),
  (
    '59e722df-feb0-4a86-bd5b-015cf118f403',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'SPECIAL',
    '5',
    'SPL',
    'SPECIAL',
    'স্পেশাল'
  ),
  (
    '59e722df-feb0-4a86-bd5b-015cf118f404',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'NON_AC',
    '10',
    'MDN',
    'MIDI NON AC',
    'মিডি নন এসি'
  ),
  (
    '59e722df-feb0-4a86-bd5b-015cf118f405',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'NON_AC',
    '7',
    'GSA',
    'GSAGAR',
    'গঙ্গা সাগর'
  ),
  (
    '59e722df-feb0-4a86-bd5b-015cf118f406',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'EXECUTIVE',
    '4',
    'EXE',
    'EXECUTIVE',
    'এক্সিকিউটিভ'
  ),
  (
    '59e722df-feb0-4a86-bd5b-015cf118f407',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'EV',
    '8',
    'ELE',
    'ELECTRIC V',
    'ইলেক্ট্রিক ভি'
  ),
  (
    '59e722df-feb0-4a86-bd5b-015cf118f408',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'ORDINARY',
    '2',
    'ORD',
    'ORDINARY',
    'সাধারণ'
  );

-- ONLY FOR LOCAL
INSERT INTO
  atlas_app.route (
    integrated_bpp_config_id,
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
    updated_at
  )
VALUES
  (
    'dabe23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'Route:1:SHUTTLE-U',
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
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  );

-- ONLY FOR LOCAL
INSERT INTO
  atlas_app.station (
    integrated_bpp_config_id,
    address,
    code,
    id,
    lat,
    lon,
    merchant_operating_city_id,
    name,
    vehicle_type,
    merchant_id,
    created_at,
    updated_at
  )
VALUES
  (
    'dabe23a5-3ce6-4c37-8b9b-41377c3c1a52',
    NULL,
    'Stop:1:18465',
    md5(random() :: text || clock_timestamp() :: text) :: uuid,
    22.5945,
    88.4728,
    'namma-yatri-0-0000-0000-00000000city',
    'ECO T PARK',
    'BUS',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    now(),
    now()
  ),
  (
    'dabe23a5-3ce6-4c37-8b9b-41377c3c1a52',
    NULL,
    'Stop:1:18024',
    md5(random() :: text || clock_timestamp() :: text) :: uuid,
    22.61831,
    88.465,
    'namma-yatri-0-0000-0000-00000000city',
    'AKANKHA MORE',
    'BUS',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    now(),
    now()
  ),
  (
    'dabe23a5-3ce6-4c37-8b9b-41377c3c1a52',
    NULL,
    'Stop:1:18304',
    md5(random() :: text || clock_timestamp() :: text) :: uuid,
    22.6218,
    88.4499,
    'namma-yatri-0-0000-0000-00000000city',
    'CITY CENTER 2',
    'BUS',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    now(),
    now()
  ),
  (
    'dabe23a5-3ce6-4c37-8b9b-41377c3c1a52',
    NULL,
    'Stop:1:18705',
    md5(random() :: text || clock_timestamp() :: text) :: uuid,
    22.6275,
    88.433,
    'namma-yatri-0-0000-0000-00000000city',
    'KAIKHALI/HALDIRAM',
    'BUS',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    now(),
    now()
  ),
  (
    'dabe23a5-3ce6-4c37-8b9b-41377c3c1a52',
    NULL,
    'Stop:1:18058',
    md5(random() :: text || clock_timestamp() :: text) :: uuid,
    22.6275,
    88.433,
    'namma-yatri-0-0000-0000-00000000city',
    'AIRPORT GATE NO1',
    'BUS',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    now(),
    now()
  );

-- ONLY FOR LOCAL
INSERT INTO
  atlas_app.route_stop_mapping (
    integrated_bpp_config_id,
    provider_code,
    route_code,
    stop_code,
    stop_name,
    stop_lat,
    stop_lon,
    sequence_num,
    time_bounds,
    vehicle_type,
    merchant_id,
    merchant_operating_city_id,
    created_at,
    updated_at
  )
VALUES
  (
    'dabe23a5-3ce6-4c37-8b9b-41377c3c1a52',
    '1',
    'Route:1:SHUTTLE-U',
    'Stop:1:18465',
    'ECO T PARK',
    22.5945,
    88.4728,
    1,
    'Unbounded',
    'BUS',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  ),
  (
    'dabe23a5-3ce6-4c37-8b9b-41377c3c1a52',
    '2',
    'Route:1:SHUTTLE-U',
    'Stop:1:18024',
    'AKANKHA MORE',
    22.61831,
    88.465,
    2,
    'Unbounded',
    'BUS',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  ),
  (
    'dabe23a5-3ce6-4c37-8b9b-41377c3c1a52',
    '3',
    'Route:1:SHUTTLE-U',
    'Stop:1:18304',
    'CITY CENTER 2',
    22.6218,
    88.4499,
    3,
    'Unbounded',
    'BUS',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  ),
  (
    'dabe23a5-3ce6-4c37-8b9b-41377c3c1a52',
    '4',
    'Route:1:SHUTTLE-U',
    'Stop:1:18705',
    'KAIKHALI/HALDIRAM',
    22.6275,
    88.433,
    4,
    'Unbounded',
    'BUS',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  ),
  (
    'dabe23a5-3ce6-4c37-8b9b-41377c3c1a52',
    '5',
    'Route:1:SHUTTLE-U',
    'Stop:1:18058',
    'AIRPORT GATE NO1',
    22.64223,
    88.43121,
    5,
    'Unbounded',
    'BUS',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  );

-- ONLY FOR LOCAL
INSERT INTO
  atlas_app.frfs_ticket_discount (
    id,
    code,
    title,
    currency,
    description,
    tnc,
    merchant_id,
    merchant_operating_city_id,
    value,
    vehicle_type,
    created_at,
    updated_at
  )
VALUES
  (
    '333e23a5-3ce6-4c37-8b9b-41377c3c1333',
    'WOMEN',
    'Women 50% Off',
    'INR',
    'Women Discount',
    '<b>Only eligible for women above 18 years old</b>',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'Percentage 50.0',
    'BUS',
    now(),
    now()
  ),
  (
    '333e23a5-3ce6-4c37-8b9b-41377c3c1444',
    'SENIORCITIZEN',
    'Senior Citizen 40% Off',
    'INR',
    'Women Discount',
    '<b>Only eligible for adults with age above 70 years</b>',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'Percentage 40.0',
    'BUS',
    now(),
    now()
  );

-- ONLY FOR LOCAL
INSERT INTO
  atlas_app.frfs_fare_policy (
    id,
    type,
    applicable_discount_ids,
    description,
    merchant_id,
    merchant_operating_city_id,
    created_at,
    updated_at
  )
VALUES
  (
    '999e23a5-3ce6-4c37-8b9b-41377c3c1999',
    'MatrixBased',
    '{"333e23a5-3ce6-4c37-8b9b-41377c3c1333", "333e23a5-3ce6-4c37-8b9b-41377c3c1444"}',
    'SHUTTLE - U | ECO T PARK - AIRPORT GATE NO1',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  );

-- ONLY FOR LOCAL
INSERT INTO
  atlas_app.route_stop_fare (
    fare_policy_id,
    route_code,
    start_stop_code,
    end_stop_code,
    amount,
    currency,
    merchant_id,
    merchant_operating_city_id,
    created_at,
    updated_at
  )
VALUES
  (
    '999e23a5-3ce6-4c37-8b9b-41377c3c1999',
    'Route:1:SHUTTLE-U',
    'Stop:1:18465',
    'Stop:1:18024',
    10,
    'INR',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  ),
  (
    '999e23a5-3ce6-4c37-8b9b-41377c3c1999',
    'Route:1:SHUTTLE-U',
    'Stop:1:18465',
    'Stop:1:18304',
    20,
    'INR',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  ),
  (
    '999e23a5-3ce6-4c37-8b9b-41377c3c1999',
    'Route:1:SHUTTLE-U',
    'Stop:1:18465',
    'Stop:1:18705',
    30,
    'INR',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  ),
  (
    '999e23a5-3ce6-4c37-8b9b-41377c3c1999',
    'Route:1:SHUTTLE-U',
    'Stop:1:18465',
    'Stop:1:18058',
    40,
    'INR',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  ),
  (
    '999e23a5-3ce6-4c37-8b9b-41377c3c1999',
    'Route:1:SHUTTLE-U',
    'Stop:1:18024',
    'Stop:1:18304',
    10,
    'INR',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  ),
  (
    '999e23a5-3ce6-4c37-8b9b-41377c3c1999',
    'Route:1:SHUTTLE-U',
    'Stop:1:18024',
    'Stop:1:18705',
    20,
    'INR',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  ),
  (
    '999e23a5-3ce6-4c37-8b9b-41377c3c1999',
    'Route:1:SHUTTLE-U',
    'Stop:1:18024',
    'Stop:1:18058',
    30,
    'INR',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  ),
  (
    '999e23a5-3ce6-4c37-8b9b-41377c3c1999',
    'Route:1:SHUTTLE-U',
    'Stop:1:18304',
    'Stop:1:18705',
    10,
    'INR',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  ),
  (
    '999e23a5-3ce6-4c37-8b9b-41377c3c1999',
    'Route:1:SHUTTLE-U',
    'Stop:1:18304',
    'Stop:1:18058',
    20,
    'INR',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  ),
  (
    '999e23a5-3ce6-4c37-8b9b-41377c3c1999',
    'Route:1:SHUTTLE-U',
    'Stop:1:18705',
    'Stop:1:18058',
    10,
    'INR',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  );

-- ONLY FOR LOCAL
INSERT INTO
  atlas_app.frfs_route_fare_product (
    integrated_bpp_config_id,
    id,
    route_code,
    fare_policy_id,
    merchant_id,
    merchant_operating_city_id,
    time_bounds,
    vehicle_service_tier_id,
    vehicle_type,
    created_at,
    updated_at
  )
VALUES
  (
    'dabe23a5-3ce6-4c37-8b9b-41377c3c1a52',
    md5(random() :: text || clock_timestamp() :: text) :: uuid,
    'Route:1:SHUTTLE-U',
    '999e23a5-3ce6-4c37-8b9b-41377c3c1999',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'Unbounded',
    '59e722df-feb0-4a86-bd5b-015cf118f400',
    'BUS',
    now(),
    now()
  );

-- ONLY FOR LOCAL
INSERT INTO
  atlas_app.app_dynamic_logic_element (
    description,
    domain,
    logic,
    "order",
    version,
    created_at,
    updated_at
  )
VALUES
  (
    'FRFS Discounts Eligibility',
    'FRFS-DISCOUNTS',
    '{ "filter" :[{"var":"discounts"},{"or":[{"and":[{"==":[{"var":"code"},"WOMEN"] },{ "==" :[{"var":"aadhaarData.personGender"},"women"] } ] },{ "!=" :[{"var":"code"},"WOMEN"] } ] } ] }',
    0,
    1,
    now(),
    now()
  );

INSERT INTO
  atlas_app.app_dynamic_logic_rollout (
    domain,
    percentage_rollout,
    version,
    version_description,
    time_bounds,
    merchant_operating_city_id,
    created_at,
    updated_at
  )
VALUES
  (
    'FRFS-DISCOUNTS',
    100,
    1,
    'FRFS Discounts Eligibility For Women',
    'Unbounded',
    'namma-yatri-0-0000-0000-00000000city',
    now(),
    now()
  );