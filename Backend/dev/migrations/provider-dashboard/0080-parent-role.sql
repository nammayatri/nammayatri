ALTER TABLE atlas_bpp_dashboard.role ADD COLUMN parent_role_id character(36) REFERENCES atlas_bpp_dashboard.role (id);

ALTER TABLE atlas_bpp_dashboard.merchant ADD COLUMN user_action_types_for_descendants_check text[];

-- ONLY FOR LOCAL --
UPDATE atlas_bpp_dashboard.merchant
SET user_action_types_for_descendants_check = ARRAY['PROVIDER_MANAGEMENT/ACCOUNT/PUT_ACCOUNT_UPDATE_ROLE']::text[]
WHERE  short_id = 'NAMMA_YATRI_PARTNER';

-- ONLY FOR LOCAL: juspay ops role
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    VALUES
    ( atlas_bpp_dashboard.uuid_generate_v4(),
      'd5644e83-ffa3-4e0d-ae81-c3155eedb8fd',
      'DSL',
      'USER_FULL_ACCESS',
      unnest
        (ARRAY
          [ 'PROVIDER_MANAGEMENT/ACCOUNT/PUT_ACCOUNT_UPDATE_ROLE'
          ]
        )
    )
    ON CONFLICT DO NOTHING;

-- ONLY FOR LOCAL: customer service role
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    VALUES
    ( atlas_bpp_dashboard.uuid_generate_v4(),
      'a708c6a1-78b5-4e5e-9df8-468cd81dc2aa',
      'DSL',
      'USER_FULL_ACCESS',
      unnest
        (ARRAY
          [ 'PROVIDER_MANAGEMENT/ACCOUNT/PUT_ACCOUNT_UPDATE_ROLE'
          ]
        )
    )
    ON CONFLICT DO NOTHING;
