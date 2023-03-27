CREATE OR REPLACE FUNCTION atlas_bpp_dashboard.uuid_generate_v4() RETURNS character (36) AS $uuid_generate_v4$
    BEGIN
        RETURN (uuid_in((md5((random())::text))::cstring));
    END;
$uuid_generate_v4$ LANGUAGE plpgsql;

ALTER TABLE atlas_bpp_dashboard.access_matrix ADD COLUMN user_action_type varchar (255);
ALTER TABLE atlas_bpp_dashboard.access_matrix DROP CONSTRAINT unique_role_id_api_entity;
ALTER TABLE atlas_bpp_dashboard.access_matrix ADD CONSTRAINT unique_role_id_api_entity_user_action_type UNIQUE (role_id, api_entity, user_action_type);

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['MERCHANT_UPDATE' ,'MAPS_SERVICE_CONFIG_UPDATE', 'MAPS_SERVICE_USAGE_CONFIG_UPDATE','SMS_SERVICE_CONFIG_UPDATE','SMS_SERVICE_USAGE_CONFIG_UPDATE', 'VERIFICATION_SERVICE_CONFIG_UPDATE'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'MERCHANT' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY [  'UPLOAD_DOCUMENT','REGISTER_DL','REGISTER_RC','ENABLE','DISABLE','BLOCK','UNBLOCK','DELETE_DRIVER','UNLINK_VEHICLE','END_RC_ASSOCIATION','UNLINK_DL','UPDATE_PHONE_NUMBER','ADD_VEHICLE','UPDATE_DRIVER_NAME'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'DRIVERS' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['DOCUMENT_LIST' ,'GET_DOCUMENT','DOCUMENTS_INFO','LIST','ACTIVITY','LOCATION','INFO'])
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
        unnest (ARRAY ['STUCK_BOOKING_CANCEL','RIDE_START','RIDE_END','RIDE_CANCEL','RIDE_SYNC'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'RIDES' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['RIDE_LIST','RIDE_ROUTE','RIDE_INFO'])
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
        unnest (ARRAY ['CUSTOMER_UPDATE','CUSTOMER_DELETE'])
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
        unnest (ARRAY ['CUSTOMER_LIST'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'CUSTOMERS' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_READ_ACCESS')
    )
ON CONFLICT DO NOTHING;


INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type)
    (
    SELECT
        atlas_bpp_dashboard.uuid_generate_v4(),
        T1.role_id,
        T1.api_entity,
        'USER_FULL_ACCESS',
        unnest (ARRAY ['UPLOAD_FILE','ADD_LINK','ADD_MESSAGE','SEND_MESSAGE'])
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
        unnest (ARRAY ['MESSAGE_LIST','MESSAGE_INFO','MESSAGE_DELIVERY_INFO','MESSAGE_RECEIVER_LIST'])
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
        unnest (ARRAY ['ISSUE_UPDATE','ISSUE_ADD_COMMENT'])
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
        unnest (ARRAY ['ISSUE_LIST'])
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
        unnest (ARRAY ['REFERRAL_PROGRAM_PASSWORD_UPDATE','REFERRAL_PROGRAM_LINK_CODE'])
    FROM atlas_bpp_dashboard.access_matrix AS T1
    WHERE T1.api_entity = 'REFERRAL' AND (T1.user_access_type = 'USER_FULL_ACCESS' OR T1.user_access_type = 'USER_WRITE_ACCESS')
    )
ON CONFLICT DO NOTHING;

DELETE FROM atlas_bpp_dashboard.access_matrix WHERE user_action_type IS NULL;
ALTER TABLE atlas_bpp_dashboard.access_matrix ALTER COLUMN user_action_type SET NOT NULL;