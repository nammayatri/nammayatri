------------------------------- Insertion Queries--------------------------------

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DriverOfferBPP (MERCHANT UPDATE)','DriverOfferBPP (MERCHANT COMMON_CONFIG_UPDATE)','DriverOfferBPP (MERCHANT DRIVER_POOL_CONFIG_UPDATE)','DriverOfferBPP (MERCHANT DRIVER_POOL_CONFIG_CREATE)','DriverOfferBPP (MERCHANT DRIVER_INTELLIGENT_POOL_CONFIG_UPDATE)','DriverOfferBPP (MERCHANT ONBOARDING_DOCUMENT_CONFIG_UPDATE)','DriverOfferBPP (MERCHANT ONBOARDING_DOCUMENT_CONFIG_CREATE)' ,'DriverOfferBPP (MERCHANT MAPS_SERVICE_CONFIG_UPDATE)', 'DriverOfferBPP (MERCHANT MAPS_SERVICE_USAGE_CONFIG_UPDATE)','DriverOfferBPP (MERCHANT SMS_SERVICE_CONFIG_UPDATE)','DriverOfferBPP (MERCHANT SMS_SERVICE_USAGE_CONFIG_UPDATE)', 'DriverOfferBPP (MERCHANT VERIFICATION_SERVICE_CONFIG_UPDATE)'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'MERCHANT' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

UPDATE atlas_bpp_dashboard.access_matrix SET user_action_type = 'DriverOfferBPP (MERCHANT COMMON_CONFIG)' where user_action_type = 'MERCHANT_COMMON_CONFIG';
UPDATE atlas_bpp_dashboard.access_matrix SET user_action_type = 'DriverOfferBPP (MERCHANT DRIVER_POOL_CONFIG)' where user_action_type = 'DRIVER_POOL_CONFIG';
UPDATE atlas_bpp_dashboard.access_matrix SET user_action_type = 'DriverOfferBPP (MERCHANT DRIVER_INTELLIGENT_POOL_CONFIG)' where user_Action_type = 'DRIVER_INTELLIGENT_POOL_CONFIG';
UPDATE atlas_bpp_dashboard.access_matrix SET user_action_type = 'DriverOfferBPP (MERCHANT ONBOARDING_DOCUMENT_CONFIG)' where user_Action_type = 'ONBOARDING_DOCUMENT_CONFIG';
UPDATE atlas_bpp_dashboard.access_matrix SET user_action_type = 'DriverOfferBPP (MERCHANT SERVICE_USAGE_CONFIG)' where user_Action_type =  'SERVICE_USAGE_CONFIG';

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DriverOfferBPP (DRIVERS UPLOAD_DOCUMENT)','DriverOfferBPP (DRIVERS REGISTER_DL)','DriverOfferBPP (DRIVERS REGISTER_RC)','DriverOfferBPP (DRIVERS ENABLE)','DriverOfferBPP (DRIVERS DISABLE)','DriverOfferBPP (DRIVERS BLOCK)','DriverOfferBPP (DRIVERS UNBLOCK)','DriverOfferBPP (DRIVERS DELETE)','DriverOfferBPP (DRIVERS UNLINK_VEHICLE)','DriverOfferBPP (DRIVERS END_RC_ASSOCIATION)','DriverOfferBPP (DRIVERS UNLINK_DL)','DriverOfferBPP (DRIVERS UPDATE_PHONE_NUMBER)','DriverOfferBPP (DRIVERS ADD_VEHICLE)','DriverOfferBPP (DRIVERS UPDATE_DRIVER_NAME)','DriverOfferBPP (DRIVERS CREATE_FP_DRIVER_EXTRA_FEE)','DriverOfferBPP (DRIVERS UPDATE_FP_DRIVER_EXTRA_FEE)'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'DRIVERS' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS') AND T1.role_id != 'a708c6a1-78b5-4e5e-9df8-468cd81dc2aa'
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DriverOfferBPP (DRIVERS DOCUMENT_LIST)' ,'DriverOfferBPP (DRIVERS GET_DOCUMENT)','DriverOfferBPP (DRIVERS DOCUMENTS_INFO)','DriverOfferBPP (DRIVERS LIST)','DriverOfferBPP (DRIVERS ACTIVITY)','DriverOfferBPP (DRIVERS LOCATION)','DriverOfferBPP (DRIVERS INFO)'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'DRIVERS' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_READ_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DriverOfferBPP (RIDES STUCK_BOOKING_CANCEL)','DriverOfferBPP (RIDES START)','DriverOfferBPP (RIDES END)','DriverOfferBPP (RIDES CANCEL)','DriverOfferBPP (RIDES SYNC)'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'RIDES' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS') AND role_id not in ('508a0bac-258d-44a6-ac55-aef57ab87a76','e5a69a26-d165-455a-a711-33a41e0d47c6')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DriverOfferBPP (RIDES LIST)','DriverOfferBPP (RIDES ROUTE)','DriverOfferBPP (RIDES INFO)'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'RIDES' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_READ_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DriverOfferBPP (CUSTOMERS DELETE)','DriverOfferBPP (CUSTOMERS BLOCK)','DriverOfferBPP (CUSTOMERS UNBLOCK)'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'CUSTOMERS' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DriverOfferBPP (CUSTOMERS LIST)','DriverOfferBPP (CUSTOMERS INFO)'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'CUSTOMERS' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_READ_ACCESS') AND T1.role_id != 'a708c6a1-78b5-4e5e-9df8-468cd81dc2aa'
    )
ON CONFLICT DO NOTHING;


INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DriverOfferBPP (MESSAGE UPLOAD_FILE)','DriverOfferBPP (MESSAGE ADD_LINK)','DriverOfferBPP (MESSAGE ADD)','DriverOfferBPP (MESSAGE SEND)'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'MESSAGE' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DriverOfferBPP (MESSAGE LIST)','DriverOfferBPP (MESSAGE INFO)','DriverOfferBPP (MESSAGE DELIVERY_INFO)','DriverOfferBPP (MESSAGE RECEIVER_LIST)'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'MESSAGE' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_READ_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DriverOfferBPP (ISSUE UPDATE)','DriverOfferBPP (ISSUE ADD_COMMENT)'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'ISSUE' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DriverOfferBPP (ISSUE LIST)','DriverOfferBPP (ISSUE CATEGORY_LIST)','DriverOfferBPP (ISSUE INFO)','DriverOfferBPP (ISSUE FETCH_MEDIA)'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'ISSUE' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_READ_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DriverOfferBPP (REFERRAL PROGRAM_PASSWORD_UPDATE)','DriverOfferBPP (REFERRAL PROGRAM_LINK_CODE)'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'REFERRAL' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DriverOfferBPP (VOLUNTEER BOOKING_INFO)'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'VOLUNTEER' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_READ_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DriverOfferBPP (VOLUNTEER ASSIGN_CREATE_AND_START_OTP_RIDE)'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'VOLUNTEER' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

UPDATE atlas_bpp_dashboard.access_matrix set user_action_type = 'SpecialZones (SPECIAL_ZONES CREATE)' where id = '2t4eb898-1410-40d3-8c1e-27c1087f4132';

UPDATE atlas_bpp_dashboard.access_matrix set user_action_type = 'SpecialZones (SPECIAL_ZONES LOOKUP)' where id = '2t4eb898-1410-40d3-8c1e-27c1087f4133';

UPDATE atlas_bpp_dashboard.access_matrix set user_action_type = 'SpecialZones (SPECIAL_ZONES UPDATE)' where id = '2t4eb898-1410-40d3-8c1e-27c1087f4134';

UPDATE atlas_bpp_dashboard.access_matrix set user_action_type = 'SpecialZones (SPECIAL_ZONES DELETE)' where id = '2t4eb898-1410-40d3-8c1e-27c1087f4135';

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
 (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DriverOfferBPP (DRIVERS CLEAR_ON_RIDE_STUCK_DRIVER_IDS)'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'DRIVERS' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

---------------------------- Alter Table queries ------------------------------------

ALTER TABLE atlas_bpp_dashboard.access_matrix DROP CONSTRAINT unique_role_id_api_entity_user_action_type;

ALTER TABLE atlas_bpp_dashboard.access_matrix ADD CONSTRAINT unique_role_id_user_action_type UNIQUE (role_id, user_action_type);

ALTER TABLE atlas_bpp_dashboard.access_matrix DROP COLUMN api_entity;

-----------------------------Deleting Old Rows --------------------------------------
DELETE FROM atlas_bpp_dashboard.access_matrix WHERE user_action_type NOT LIKE '%DriverOfferBPP (%';