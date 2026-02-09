-- Add BHARAT_TAXI to server_names for the BHARAT_TAXI merchant
UPDATE atlas_bap_dashboard.merchant
SET server_names = array_append(server_names, 'BHARAT_TAXI')
WHERE short_id = 'BHARAT_TAXI'
  AND NOT ('BHARAT_TAXI' = ANY(server_names));

-- Add BHARAT_TAXI_USER access matrix entries for the BHARAT_TAXI merchant
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'BHARAT_TAXI_USER', 'USER_FULL_ACCESS', 'BHARAT_TAXI_FROM_LIST', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'BHARAT_TAXI_USER', 'USER_FULL_ACCESS', 'BHARAT_TAXI_TO_LIST', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'BHARAT_TAXI_USER', 'USER_FULL_ACCESS', 'BHARAT_TAXI_ESTIMATE', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'BHARAT_TAXI_USER', 'USER_FULL_ACCESS', 'BHARAT_TAXI_BOOKING', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'BHARAT_TAXI_USER', 'USER_FULL_ACCESS', 'BHARAT_TAXI_INVOICE', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'BHARAT_TAXI_USER', 'USER_FULL_ACCESS', 'BHARAT_TAXI_BOOKING_LATEST', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'BHARAT_TAXI_USER', 'USER_FULL_ACCESS', 'BHARAT_TAXI_BOOKING_BY_ID', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'BHARAT_TAXI_USER', 'USER_FULL_ACCESS', 'BHARAT_TAXI_UPDATE_BOOKING', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'BHARAT_TAXI_USER', 'USER_FULL_ACCESS', 'BHARAT_TAXI_VEHICLES_LIST', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'BHARAT_TAXI_USER', 'USER_FULL_ACCESS', 'BHARAT_TAXI_VEHICLES_CREATE', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'BHARAT_TAXI_USER', 'USER_FULL_ACCESS', 'BHARAT_TAXI_DRIVERS_LIST', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'BHARAT_TAXI_USER', 'USER_FULL_ACCESS', 'BHARAT_TAXI_DRIVERS_CREATE', now(), now())
ON CONFLICT (role_id, api_entity, user_action_type) DO NOTHING;