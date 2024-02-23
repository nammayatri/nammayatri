INSERT INTO atlas_app.beckn_config (id, domain, gateway_url, registry_url, subscriber_id, subscriber_url, merchant_id, merchant_operating_city_id, unique_key_id, created_at, updated_at, vehicle_category)
VALUES (
    'dd22a05d-29a3-42c8-9c8d-2de340f9b710',
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
    'AUTO_RICKSHAW'
), ('dd22a05d-29a3-42c8-9c8d-2de340f9b709',
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
    'CAB'
);

UPDATE atlas_app.beckn_config
SET payment_params_json = '{"bankAccNumber": "xyz@upi","bankCode": "xyz"}'
WHERE domain = 'MOBILITY';
