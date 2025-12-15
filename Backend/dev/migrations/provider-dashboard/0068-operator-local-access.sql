-- ONLY FOR LOCAL: operator role
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    VALUES
    ( atlas_bpp_dashboard.uuid_generate_v4(),
      '00000000-0000-0000-000-operator-role',
      'DSL',
      'USER_FULL_ACCESS',
      unnest
        (ARRAY
          [ 'PROVIDER_OPERATOR/FLEET_MANAGEMENT/POST_FLEET_MANAGEMENT_FLEET_LINK_SEND_OTP',
            'PROVIDER_OPERATOR/FLEET_MANAGEMENT/POST_FLEET_MANAGEMENT_FLEET_LINK_VERIFY_OTP',
            'PROVIDER_OPERATOR/DRIVER/POST_DRIVER_OPERATOR_SEND_JOINING_OTP',
            'PROVIDER_OPERATOR/DRIVER/POST_DRIVER_OPERATOR_VERIFY_JOINING_OTP',
            'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_DRIVERS',
            'PROVIDER_OPERATOR/DRIVER/POST_DRIVER_OPERATOR_RESPOND_HUB_REQUEST',
            'PROVIDER_FLEET/REGISTRATION_V2/POST_REGISTRATION_V2_REGISTER',
            'PROVIDER_FLEET/LIVE_MAP/GET_LIVE_MAP_DRIVERS',
            'PROVIDER_MANAGEMENT/MEDIA_FILE_DOCUMENT/POST_MEDIA_FILE_DOCUMENT_UPLOAD_LINK',
            'PROVIDER_MANAGEMENT/MEDIA_FILE_DOCUMENT/POST_MEDIA_FILE_DOCUMENT_CONFIRM',
            'PROVIDER_MANAGEMENT/MEDIA_FILE_DOCUMENT/POST_MEDIA_FILE_DOCUMENT_DELETE',
            'PROVIDER_MANAGEMENT/MEDIA_FILE_DOCUMENT/GET_MEDIA_FILE_DOCUMENT_DOWNLOAD_LINK'
          ]
        )
    )
    ON CONFLICT DO NOTHING;

-- ONLY FOR LOCAL: fleet owner role
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    VALUES
    ( atlas_bpp_dashboard.uuid_generate_v4(),
      'e5a69a26-d165-455a-a711-33a41e0d4812',
      'DSL',
      'USER_FULL_ACCESS',
      unnest
        (ARRAY
          [ 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_DRIVERS',
            'PROVIDER_FLEET/REGISTRATION_V2/POST_REGISTRATION_V2_REGISTER',
            'PROVIDER_FLEET/LIVE_MAP/GET_LIVE_MAP_DRIVERS',
            'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_STATS',
            'PROVIDER_FLEET/REGISTRATION_V2/POST_REGISTRATION_V2_REGISTER_BANK_ACCOUNT_LINK',
            'PROVIDER_FLEET/DRIVER/POST_DRIVER_UPDATE_FLEET_OWNER_INFO'
          ]
        )
    )
    ON CONFLICT DO NOTHING;

-- ONLY FOR LOCAL: rental fleet owner role
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    VALUES
    ( atlas_bpp_dashboard.uuid_generate_v4(),
      '00000000-0000-0000-rental-fleet-role',
      'DSL',
      'USER_FULL_ACCESS',
      unnest
        (ARRAY
          [ 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_DRIVERS',
            'PROVIDER_FLEET/REGISTRATION_V2/POST_REGISTRATION_V2_REGISTER',
            'PROVIDER_FLEET/LIVE_MAP/GET_LIVE_MAP_DRIVERS'
          ]
        )
    )
    ON CONFLICT DO NOTHING;

-- ONLY FOR LOCAL:
UPDATE atlas_bpp_dashboard.merchant
SET has_fleet_member_hierarchy='false' WHERE short_id='NAMMA_YATRI_PARTNER';