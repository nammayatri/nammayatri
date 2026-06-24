-- Rewards dashboard integration tests: access_matrix + enable flag for NY/BT.
-- Idempotent — safe to re-run before ./run-tests.sh rewards

-- Access matrix (JUSPAY_ADMIN role from rider-dashboard.sql)
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/REWARDS/POST_REWARDS_CAMPAIGN' )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/REWARDS/PUT_REWARDS_CAMPAIGN' )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/REWARDS/POST_REWARDS_CAMPAIGN_COHORT' )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/REWARDS/PUT_REWARDS_CAMPAIGN_COHORT' )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/REWARDS/POST_REWARDS_CAMPAIGN_COHORT_CODES' )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/REWARDS/POST_REWARDS_CAMPAIGN_STATUS' )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/REWARDS/GET_REWARDS_CAMPAIGN' )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/REWARDS/GET_REWARDS_CAMPAIGNS' )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/REWARDS/GET_REWARDS_CAMPAIGN_STATS' )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/REWARDS/POST_REWARDS_TRIGGER_EVAL' )
ON CONFLICT DO NOTHING;

-- Enable rewards management for Namma Yatri + Bharat Taxi (all operating cities).
UPDATE atlas_app.rider_config rc
SET enable_rewards_management = true
FROM atlas_app.merchant_operating_city moc
JOIN atlas_app.merchant m ON m.id = moc.merchant_id
WHERE rc.merchant_operating_city_id = moc.id
  AND m.short_id IN ('NAMMA_YATRI', 'BHARAT_TAXI');
