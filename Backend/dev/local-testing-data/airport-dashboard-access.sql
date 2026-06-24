-- Airport taxi booth setup for BHARAT_TAXI Delhi integration tests (idempotent).
-- Applied by test-context-api startup (local-testing-data batch).
--
-- Fixes three issues that prevent AirportTaxiFlow from working locally:
-- 1. access_matrix entries for DSL-based rideBooking endpoints
-- 2. white_list_org entry for BHARAT_TAXI_PARTNER localhost subscriber
--    (config-sync seeds the production URL, not localhost)
-- 3. OSRM→Google routing switch for Delhi (local OSRM only has Bangalore data,
--    returns distance=0 for Delhi coordinates → empty quotes)

-- Ride Search
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/SEARCH/POST_SEARCH_RIDE')
ON CONFLICT DO NOTHING;

-- Get Quotes
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/QUOTE/GET_QUOTE_RESULT')
ON CONFLICT DO NOTHING;

-- Rider Registration Auth
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_AUTH')
ON CONFLICT DO NOTHING;

-- Rider Registration Verify
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_VERIFY')
ON CONFLICT DO NOTHING;

-- Update Rider Profile
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/PROFILE/POST_PROFILE_UPDATE')
ON CONFLICT DO NOTHING;

-- Confirm Booking
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/CONFIRM/POST_CONFIRM_RIDE_SEARCH_QUOTES')
ON CONFLICT DO NOTHING;

-- Get Booking Details
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/BOOKING/POST_BOOKING_STATUS')
ON CONFLICT DO NOTHING;

-- ============================================================
-- 2. Whitelist BHARAT_TAXI_PARTNER localhost subscriber
-- ============================================================
-- Config-sync seeds the production URL (api.sandbox.moving.tech/...)
-- but locally the driver-app uses localhost:8016. Without this entry
-- the rider-app rejects on_search callbacks from BHARAT_TAXI_PARTNER.

INSERT INTO atlas_app.white_list_org (id, subscriber_id, domain, merchant_id, merchant_operating_city_id)
VALUES (
  gen_random_uuid(),
  'localhost:8016/beckn/7e3d0f76-4c63-4b38-93c4-29b1e508fa21',
  'MOBILITY',
  'c91a2f44-3a7e-4e86-8c1e-2c4f3b0b4c98',
  '1e84858a-4deb-45d6-a97b-7c7504358548'
) ON CONFLICT DO NOTHING;

-- ============================================================
-- 3. Switch OSRM → Google for Delhi (both rider-app and driver-app)
-- ============================================================
-- Local OSRM only has Bangalore map data. Delhi coordinates return
-- distance=0 → driver-app can't compute fare → empty quotes.
-- Google mock (port 8080) returns 5km/10min for any request.

UPDATE atlas_app.merchant_service_usage_config
SET get_routes = 'Google', get_distances = 'Google'
WHERE merchant_operating_city_id = '1e84858a-4deb-45d6-a97b-7c7504358548';

UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET get_routes = 'Google', get_distances = 'Google'
WHERE merchant_operating_city_id = 'ef75e743-53a6-4040-af57-a6036ab091ab';
