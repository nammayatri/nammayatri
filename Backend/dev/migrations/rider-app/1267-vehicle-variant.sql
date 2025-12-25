-- RUN ONLY THIS insert query in master
INSERT INTO atlas_app.beckn_config (
    Domain, Gateway_URL, ID, Registry_URL, Subscriber_ID, Subscriber_URL, Unique_Key_ID, Merchant_ID, Created_At, Updated_At, Payment_Params_Json, Init_Ttl_Sec, Confirm_Ttl_Sec, Vehicle_Category, Collected_By, Select_Ttl_Sec, Search_Ttl_Sec, Confirm_Buffer_Ttl_Sec, Track_Ttl_Sec, Status_Ttl_Sec, Rating_Ttl_Sec, Cancel_Ttl_Sec
)
SELECT
    'MOBILITY',
    'https://beta.beckn.uat.juspay.net/dev/gateway/v1',
    md5('7f038a31-a01b-4730-aa74-a4ca62e20cf1' || CURRENT_TIMESTAMP || m.id) :: uuid,
    'https://beta.beckn.uat.juspay.net/dev/registry',
    'api.sandbox.beckn.juspay.in/dev/bap/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a51',
    'https://api.sandbox.beckn.juspay.in/dev/bap/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a51',
    54,
    m.merchant_id,
    '2024-02-23T09:41:59.574207Z',
    '2024-02-23T09:41:59.574207Z',
    '{"bankAccNumber": "xyz@upi","bankCode": "xyz"}',
    120,
    120,
    'MOTORCYCLE',
    'BPP',
    120,
    120,
    10,
    10,
    30,
    30,
    3
FROM atlas_app.merchant_operating_city as m where city = 'Siliguri';

-- INSERT into bap beckn_config ONLY-FOR-LOCAL-TESTING
INSERT INTO
    atlas_app.beckn_config (
        id,
        domain,
        gateway_url,
        registry_url,
        subscriber_id,
        subscriber_url,
        merchant_id,
        merchant_operating_city_id,
        unique_key_id,
        created_at,
        updated_at,
        vehicle_category,
        search_ttl_sec,
        select_ttl_sec,
        init_ttl_sec,
        confirm_ttl_sec,
        confirm_buffer_ttl_sec,
        track_ttl_sec,
        status_ttl_sec,
        rating_ttl_sec,
        cancel_ttl_sec,
        payment_params_json
    )
VALUES
  (
        'dd22a05d-29a3-42c8-9c8d-2de340f9b713',
        'MOBILITY',
        'http://localhost:8015/v1',
        'http://localhost:8020',
        'localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
        'http://localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
        'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
        null,
        'localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
        now(),
        now(),
        'MOTORCYCLE',
        120,
        120,
        120,
        120,
        10,
        30,
        30,
        120,
        30,
        '{"bankAccNumber": "xyz@upi","bankCode": "xyz"}'
    );