------- Adding New Rows ------------------

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['AppBackendBAP (MERCHANT UPDATE)','AppBackendBAP (MERCHANT COMMON_CONFIG_UPDATE)','AppBackendBAP (MERCHANT DRIVER_POOL_CONFIG_UPDATE)','AppBackendBAP (MERCHANT DRIVER_POOL_CONFIG_CREATE)','AppBackendBAP (MERCHANT DRIVER_INTELLIGENT_POOL_CONFIG_UPDATE)','AppBackendBAP (MERCHANT ONBOARDING_DOCUMENT_CONFIG_UPDATE)','AppBackendBAP (MERCHANT ONBOARDING_DOCUMENT_CONFIG_CREATE)' ,'AppBackendBAP (MERCHANT MAPS_SERVICE_CONFIG_UPDATE)', 'AppBackendBAP (MERCHANT MAPS_SERVICE_USAGE_CONFIG_UPDATE)','AppBackendBAP (MERCHANT SMS_SERVICE_CONFIG_UPDATE)','AppBackendBAP (MERCHANT SMS_SERVICE_USAGE_CONFIG_UPDATE)', 'AppBackendBAP (MERCHANT VERIFICATION_SERVICE_CONFIG_UPDATE)'])
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'MERCHANT' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['AppBackendBAP (DRIVERS UPLOAD_DOCUMENT)','AppBackendBAP (DRIVERS REGISTER_DL)','AppBackendBAP (DRIVERS REGISTER_RC)','AppBackendBAP (DRIVERS ENABLE)','AppBackendBAP (DRIVERS DISABLE)','AppBackendBAP (DRIVERS BLOCK)','AppBackendBAP (DRIVERS UNBLOCK)','AppBackendBAP (DRIVERS DELETE)','AppBackendBAP (DRIVERS UNLINK_VEHICLE)','AppBackendBAP (DRIVERS END_RC_ASSOCIATION)','AppBackendBAP (DRIVERS UNLINK_DL)','AppBackendBAP (DRIVERS UPDATE_PHONE_NUMBER)','AppBackendBAP (DRIVERS ADD_VEHICLE)','AppBackendBAP (DRIVERS UPDATE_DRIVER_NAME)'])
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'DRIVERS' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS') AND T1.role_id != 'a708c6a1-78b5-4e5e-9df8-468cd81dc2aa'
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['AppBackendBAP (DRIVERS DOCUMENT_LIST)' ,'AppBackendBAP (DRIVERS GET_DOCUMENT)','AppBackendBAP (DRIVERS DOCUMENTS_INFO)','AppBackendBAP (DRIVERS LIST)','AppBackendBAP (DRIVERS ACTIVITY)','AppBackendBAP (DRIVERS LOCATION)','AppBackendBAP (DRIVERS INFO)'])
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'DRIVERS' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_READ_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['AppBackendBAP (RIDES STUCK_BOOKING_CANCEL)','AppBackendBAP (RIDES START)','AppBackendBAP (RIDES END)','AppBackendBAP (RIDES CANCEL)','AppBackendBAP (RIDES SYNC)'])
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'RIDES' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS') AND T1.role_id NOT IN ('508a0bac-258d-44a6-ac55-aef57ab87a76','e5a69a26-d165-455a-a711-33a41e0d47c6')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['AppBackendBAP (RIDES LIST)','AppBackendBAP (RIDES ROUTE)','AppBackendBAP (RIDES INFO)'])
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'RIDES' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_READ_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['AppBackendBAP (CUSTOMERS DELETE)','AppBackendBAP (CUSTOMERS BLOCK)','AppBackendBAP (CUSTOMERS UNBLOCK)'])
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'CUSTOMERS' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS') AND T1.role_id != 'a708c6a1-78b5-4e5e-9df8-468cd81dc2aa'
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['AppBackendBAP (CUSTOMERS LIST)','AppBackendBAP (CUSTOMERS INFO)'])
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'CUSTOMERS' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_READ_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['AppBackendBAP (MESSAGE UPLOAD_FILE)','AppBackendBAP (MESSAGE ADD_LINK)','AppBackendBAP (MESSAGE ADD)','AppBackendBAP (MESSAGE SEND)'])
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'MESSAGE' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['AppBackendBAP (MESSAGE LIST)','AppBackendBAP (MESSAGE INFO)','AppBackendBAP (MESSAGE DELIVERY_INFO)','AppBackendBAP (MESSAGE RECEIVER_LIST)'])
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'MESSAGE' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_READ_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['AppBackendBAP (ISSUE UPDATE)','AppBackendBAP (ISSUE ADD_COMMENT)'])
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'ISSUE' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['AppBackendBAP (ISSUE LIST)','AppBackendBAP (ISSUE CATEGORY_LIST)','AppBackendBAP (ISSUE INFO)','AppBackendBAP (ISSUE FETCH_MEDIA)'])
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'ISSUE' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_READ_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['AppBackendBAP (REFERRAL PROGRAM_PASSWORD_UPDATE)','AppBackendBAP (REFERRAL PROGRAM_LINK_CODE)'])
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'REFERRAL' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['AppBackendBAP (VOLUNTEER BOOKING_INFO)'])
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'VOLUNTEER' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_READ_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['AppBackendBAP (VOLUNTEER ASSIGN_CREATE_AND_START_OTP_RIDE)'])
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'VOLUNTEER' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bap_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        'AppBackendBAP (MERCHANT SERVICE_USAGE_CONFIG)'
    FROM atlas_bap_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'MERCHANT' AND T1.user_access_type = 'USER_FULL_ACCESS' AND T1.user_action_type = 'AppBackendBAP (MERCHANT MAPS_SERVICE_USAGE_CONFIG_UPDATE)'
    )
ON CONFLICT DO NOTHING;

UPDATE atlas_bap_dashboard.access_matrix SET user_action_type = 'AppBackendBAP (RIDES FORCE_SYNC)' where user_action_type = 'RIDE_FORCE_SYNC';

------------Alter queries ------------------

ALTER TABLE atlas_bap_dashboard.access_matrix DROP CONSTRAINT unique_role_id_api_entity_user_action_type;

ALTER TABLE atlas_bap_dashboard.access_matrix DROP COLUMN api_entity;

ALTER TABLE atlas_bap_dashboard.access_matrix ADD CONSTRAINT unique_role_id_user_action_type UNIQUE (role_id, user_action_type);

-----------------------------Deleting Old Rows ------------------------
DELETE FROM atlas_bap_dashboard.access_matrix WHERE user_action_type NOT LIKE '%AppBackendBAP (%';