-- QUERIES FOR LOCAL ONLY
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'http://localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = 'favorit0-0000-0000-0000-00000favorit';

-- QUERIES FOR MASTER ONLY, PROD QUERIES AT BOTTOM OF FILE

-- YATRI SAATHI -> YATRI SAATHI PARTNER -> KOLKATA AND SILIGURI (AND HYDERABAD..WHY?)
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'api.sandbox.beckn.juspay.in/dev/bap/beckn/cab/v1/bd064716-ae7e-48e9-85bf-282fb310209c',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f';

-- PASSCULTURE -> PASSCULTURE PARTNER -> PARIS
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'api.sandbox.beckn.juspay.in/dev/bap/beckn/cab/v1/3f20ff7b-6a97-402d-a147-8a6fb76a4b3b',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = 'fd578aea-7e4e-4b10-8a2e-7b9d5e51634e';

-- YATRI -> NY PARTNER -> KOCHI
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'api.sandbox.beckn.juspay.in/dev/bap/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a51',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.id = '1984f6b4-95eb-4683-b6b5-251b1b008566';

-- MOBILITY_REDBUS -> NY PARTNER -> ALL NY CITIES
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'api.sandbox.beckn.juspay.in/dev/bap/beckn/9e26fb47-ffc1-47a5-beb8-4b80142c9236',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';

-- MOBILITY_PAYTM -> NY PARTNER -> ALL NY CITIES
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'api.sandbox.beckn.juspay.in/dev/bap/beckn/11c05e23-f035-46fd-98a7-572dc8934323',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';

INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'api.sandbox.moving.tech/dev/bap/beckn/11c05e23-f035-46fd-98a7-572dc8934323',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';

-- NY -> NY PARTNER -> ALL NY CITIES
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'api.sandbox.beckn.juspay.in/dev/bap/beckn/cab/v1/4b17bd06-ae7e-48e9-85bf-282fb310209c',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';

-- REDBUS -> NY PARTNER -> ALL NY CITIES
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'rb-ondc-auto-stage.redbus.in',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';

-- YAARY -> NY PARTNER -> ALL NY CITIES
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'api.sandbox.beckn.juspay.in/dev/bap/beckn/cab/v1/4b17bd06-ae7e-48e9-85bf-282fb310209c',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';

-- CHARTR -> NY PARTNER -> ALL NY CITIES
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'dev-mm-ondc-api.chartr.in',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';

-- BRIDGE -> BRIDGE PARTNER -> MINNEAPOLIS
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'api.sandbox.beckn.juspay.in/dev/bap/beckn/cab/v1/e39cb491-03a3-4341-831b-b256ef3c95c9',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = 'b7269e46-933a-40c0-b636-7903d29a31b4';


-- CHANGE NULLS TO EMPTY STRINGS
UPDATE atlas_driver_offer_bpp.white_list_org SET merchant_id = '' WHERE merchant_id IS NULL;
UPDATE atlas_driver_offer_bpp.white_list_org SET merchant_operating_city_id = '' WHERE merchant_operating_city_id IS NULL;

-- QUERIES FOR PROD
-- MOBILITY REDBUS -> NY PARTNER -> ALL NY CITIES
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'api.beckn.juspay.in/bap/beckn/v1/9e26fb47-ffc1-47a5-beb8-4b80142c9236',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';

-- MOBILITY PAYTM -> NY PARTNER -> ALL NY CITIES
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'api.beckn.juspay.in/bap/beckn/v1/11c05e23-f035-46fd-98a7-572dc8934323',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';

-- NY -> NY PARTNER, Y PARTNER -> ALL NY CITIES
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'api.beckn.juspay.in/bap/beckn/v1/4b17bd06-ae7e-48e9-85bf-282fb310209c',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id IN  ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f',
                            '2e8eac28-9854-4f5d-aea6-a2f6502cfe37');

-- Y -> NY PARTNER AND Y PARTNER -> ALL NY AND Y CITIES RESPECTIVELY
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'api.beckn.juspay.in/bap/beckn/v1/c9811842-d572-11ed-afa1-0242ac120002',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id IN  ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f',
                            '2e8eac28-9854-4f5d-aea6-a2f6502cfe37');

-- YS -> YS PARTNER -> ALL YS CITIES
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'api.beckn.juspay.in/bap/beckn/v1/bd064716-ae7e-48e9-85bf-282fb310209c',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = 'd2929b92-8b12-4e21-9efd-d6203940c4c5';

-- REDBUS -> NY PARTNER -> ALL NY CITIES
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
  'rb-ondc-auto-stage.redbus.in',
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';

-- YAARY -> NY PARTNER -> ALL NY CITIES
INSERT INTO atlas_driver_offer_bpp.white_list_org
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
FROM atlas_driver_offer_bpp.merchant_operating_city city
WHERE city.merchant_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';