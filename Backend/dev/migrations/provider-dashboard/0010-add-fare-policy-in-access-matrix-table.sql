INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, created_at, updated_at, user_action_type) VALUES
    ('aef13d16-8601-1d2c-084d-eebc1f476a24', '37947162-3b5d-4ed6-bcac-08841be1534d', 'DRIVERS', 'USER_FULL_ACCESS', now(), now(), 'CLEAR_ON_RIDE_STUCK_DRIVER_IDS');

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, created_at, updated_at, user_action_type) VALUES
    ('286af0ed-b502-202e-5664-6e93074bb712', '37947162-3b5d-4ed6-bcac-08841be1534d', 'MERCHANT', 'USER_FULL_ACCESS', now(), now(), 'CREATE_FP_DRIVER_EXTRA_FEE');

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, created_at, updated_at, user_action_type) VALUES
    ('8db8df35-4ac0-34ff-94eb-5eb0ce013825', '37947162-3b5d-4ed6-bcac-08841be1534d', 'MERCHANT', 'USER_FULL_ACCESS', now(), now(), 'UPDATE_FP_DRIVER_EXTRA_FEE');
