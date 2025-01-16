-- QUERIES FOR LOCAL ONLY
INSERT INTO atlas_app.white_list_org
  ( created_at,
    domain,
    id,
    merchant_id,
    merchant_operating_city_id,
    subscriber_id,
    updated_at
  )
SELECT
  now(),
  'MOBILITY',
  md5(
  random() :: text || clock_timestamp() :: text || city.id :: text) :: uuid,
  city.merchant_id,
  city.id,
  'http://localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
  now()
FROM atlas_app.merchant_operating_city city
WHERE city.merchant_id = 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52';

INSERT INTO atlas_app.white_list_org
  ( created_at,
    domain,
    id,
    merchant_id,
    merchant_operating_city_id,
    subscriber_id,
    updated_at
  )
SELECT
  now(),
  'MOBILITY',
  md5(
  random() :: text || clock_timestamp() :: text || city.id :: text) :: uuid,
  city.merchant_id,
  city.id,
  'http://localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a51',
  now()
FROM atlas_app.merchant_operating_city city
WHERE city.merchant_id = 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';

-- QUERIES FOR MASTER ONLY, PROD QUERIES AT BOTTOM OF FILE


-- YATRI SAATHI -> YATRI SAATHI PARTNER -> KOLKATA AND SILIGURI
INSERT INTO atlas_app.white_list_org
  ( created_at,
    domain,
    id,
    merchant_id,
    merchant_operating_city_id,
    subscriber_id,
    updated_at
  )
SELECT
  now(),
  'MOBILITY',
  md5(random()::text || clock_timestamp()::text || city.id::text)::uuid,
  city.merchant_id,
  city.id,
  'api.sandbox.beckn.juspay.in/dev/dobpp/beckn/96dd7f78-787e-4a0b-8675-e9e6fe93bb8f',
  now()
FROM atlas_app.merchant_operating_city city
WHERE city.merchant_id = 'bd064716-ae7e-48e9-85bf-282fb310209c';

-- NAMMA YATRI -> NY PARTNER -> 11 CITIES
INSERT INTO atlas_app.white_list_org
  ( created_at,
    domain,
    id,
    merchant_id,
    merchant_operating_city_id,
    subscriber_id,
    updated_at
  )
SELECT
  now(),
  'MOBILITY',
  md5(random()::text || clock_timestamp()::text || city.id::text)::uuid,
  city.merchant_id,
  city.id,
  'api.sandbox.beckn.juspay.in/dev/dobpp/beckn/7f7896dd-787e-4a0b-8675-e9e6fe93bb8f',
  now()
FROM atlas_app.merchant_operating_city city
WHERE city.merchant_id IN ('4b17bd06-ae7e-48e9-85bf-282fb310209c',
                           '11c05e23-f035-46fd-98a7-572dc8934323',
                           '9e26fb47-ffc1-47a5-beb8-4b80142c9236');

INSERT INTO atlas_app.white_list_org
  ( created_at,
    domain,
    id,
    merchant_id,
    merchant_operating_city_id,
    subscriber_id,
    updated_at
  )
SELECT
  now(),
  'MOBILITY',
  md5(random()::text || clock_timestamp()::text || city.id::text)::uuid,
  city.merchant_id,
  city.id,
  'api.sandbox.moving.tech/dev/dobpp/beckn/7f7896dd-787e-4a0b-8675-e9e6fe93bb8f',
  now()
FROM atlas_app.merchant_operating_city city
WHERE city.merchant_id IN ('4b17bd06-ae7e-48e9-85bf-282fb310209c',
                           '11c05e23-f035-46fd-98a7-572dc8934323',
                           '9e26fb47-ffc1-47a5-beb8-4b80142c9236');

-- BRIDGE -> BRIDGE PARTNER -> 1 CITY (MINNEAPOLIS)
INSERT INTO atlas_app.white_list_org
  ( created_at,
    domain,
    id,
    merchant_id,
    merchant_operating_city_id,
    subscriber_id,
    updated_at
  )
SELECT
  now(),
  'MOBILITY',
  md5(random()::text || clock_timestamp()::text || city.id::text)::uuid,
  city.merchant_id,
  city.id,
  'api.sandbox.moving.tech/dev/dobpp/beckn/b7269e46-933a-40c0-b636-7903d29a31b4',
  now()
FROM atlas_app.merchant_operating_city city
WHERE city.merchant_id = 'e39cb491-03a3-4341-831b-b256ef3c95c9';

-- REST
INSERT INTO atlas_app.white_list_org
  ( created_at,
    domain,
    id,
    merchant_id,
    merchant_operating_city_id,
    subscriber_id,
    updated_at
  )
SELECT
  now(),
  'MOBILITY',
  md5(random()::text || clock_timestamp()::text || city.id::text)::uuid,
  city.merchant_id,
  city.id,
  'api.sandbox.beckn.juspay.in/dev/dobpp/beckn/2e8eac28-9854-4f5d-aea6-a2f6502cfe37',
  now()
FROM atlas_app.merchant_operating_city city
WHERE city.merchant_id IN ('4b17bd06-ae7e-48e9-85bf-282fb310209c',
                           'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51');

-- CHANGE NULLS TO EMPTY STRINGS
UPDATE atlas_app.white_list_org SET merchant_id = '' WHERE merchant_id IS NULL;
UPDATE atlas_app.white_list_org SET merchant_operating_city_id = '' WHERE merchant_operating_city_id IS NULL;

-- QUERIES FOR PROD
-- YATRI PARTNER -> YATRI AND NY -> ALL CITIES FOR RESPECTIVE BAPs
INSERT INTO atlas_app.white_list_org
  ( created_at,
    domain,
    id,
    merchant_id,
    merchant_operating_city_id,
    subscriber_id,
    updated_at
  )
SELECT
  now(),
  'MOBILITY',
  md5(random()::text || clock_timestamp()::text || city.id::text)::uuid,
  city.merchant_id,
  city.id,
  'api.beckn.juspay.in/dobpp/beckn/2e8eac28-9854-4f5d-aea6-a2f6502cfe37',
  now()
FROM atlas_app.merchant_operating_city city
WHERE city.merchant_id IN  ('c9811842-d572-11ed-afa1-0242ac120002',
                            '4b17bd06-ae7e-48e9-85bf-282fb310209c');

-- YS PARTNER -> YS -> YS CITIES
INSERT INTO atlas_app.white_list_org
  ( created_at,
    domain,
    id,
    merchant_id,
    merchant_operating_city_id,
    subscriber_id,
    updated_at
  )
SELECT
  now(),
  'MOBILITY',
  md5(random()::text || clock_timestamp()::text || city.id::text)::uuid,
  city.merchant_id,
  city.id,
  'api.beckn.juspay.in/dobpp/beckn/d2929b92-8b12-4e21-9efd-d6203940c4c5',
  now()
FROM atlas_app.merchant_operating_city city
WHERE city.merchant_id = 'bd064716-ae7e-48e9-85bf-282fb310209c';

-- NY PARTNER -> NY, Y, MOBILITY_PAYTM AND MOBILITY_REDBUS -> RESPECTIVE CITIES
INSERT INTO atlas_app.white_list_org
  ( created_at,
    domain,
    id,
    merchant_id,
    merchant_operating_city_id,
    subscriber_id,
    updated_at
  )
SELECT
  now(),
  'MOBILITY',
  md5(random()::text || clock_timestamp()::text || city.id::text)::uuid,
  city.merchant_id,
  city.id,
  'api.beckn.juspay.in/dobpp/beckn/7f7896dd-787e-4a0b-8675-e9e6fe93bb8f',
  now()
FROM atlas_app.merchant_operating_city city
WHERE city.merchant_id IN ('4b17bd06-ae7e-48e9-85bf-282fb310209c',
                           '9e26fb47-ffc1-47a5-beb8-4b80142c9236',
                           '11c05e23-f035-46fd-98a7-572dc8934323',
                           'c9811842-d572-11ed-afa1-0242ac120002');

-- YAARY PARTNER -> NY -> NY CITIES
INSERT INTO atlas_app.white_list_org
  ( created_at,
    domain,
    id,
    merchant_id,
    merchant_operating_city_id,
    subscriber_id,
    updated_at
  )
SELECT
  now(),
  'MOBILITY',
  md5(random()::text || clock_timestamp()::text || city.id::text)::uuid,
  city.merchant_id,
  city.id,
  'openbox.triffy.in',
  now()
FROM atlas_app.merchant_operating_city city
WHERE city.merchant_id = '4b17bd06-ae7e-48e9-85bf-282fb310209c';

-- CHANGE NULLS TO EMPTY STRINGS
UPDATE atlas_app.white_list_org SET merchant_id = '' WHERE merchant_id IS NULL;
UPDATE atlas_app.white_list_org SET merchant_operating_city_id = '' WHERE merchant_operating_city_id IS NULL;