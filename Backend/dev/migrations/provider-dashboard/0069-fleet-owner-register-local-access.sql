-- ONLY FOR LOCAL: access for FLEET, RENTAL_FLEET and OPERATOR role
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES
  ( atlas_bpp_dashboard.uuid_generate_v4(),
    unnest (ARRAY['e5a69a26-d165-455a-a711-33a41e0d4812', '00000000-0000-0000-rental-fleet-role', '00000000-0000-0000-000-operator-role']),
    'FLEET',
    'USER_FULL_ACCESS',
    'REGISTER_FLEET_OWNER'
  )
  ON CONFLICT DO NOTHING;
