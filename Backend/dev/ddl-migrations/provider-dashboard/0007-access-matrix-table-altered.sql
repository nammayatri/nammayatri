CREATE OR REPLACE FUNCTION atlas_bpp_dashboard.uuid_generate_v4() RETURNS character (36) AS $uuid_generate_v4$
    BEGIN
        RETURN (uuid_in((md5((random())::text))::cstring));
    END;
$uuid_generate_v4$ LANGUAGE plpgsql;

ALTER TABLE atlas_bpp_dashboard.access_matrix ADD COLUMN user_action_type varchar (255);
ALTER TABLE atlas_bpp_dashboard.access_matrix DROP CONSTRAINT unique_role_id_api_entity;
ALTER TABLE atlas_bpp_dashboard.access_matrix ADD CONSTRAINT unique_role_id_api_entity_user_action_type UNIQUE (role_id, api_entity, user_action_type);
ALTER TABLE atlas_bpp_dashboard.access_matrix ALTER COLUMN user_action_type SET NOT NULL;
