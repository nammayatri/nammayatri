
INSERT INTO atlas_bpp_dashboard.merchant (id, short_id, server_name, created_at) VALUES
    ('sz2db186-39d3-48a4-ad1f-78a0c3f840fd', 'SPECIAL_ZONE', 'SPECIAL_ZONE', now ());

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, created_at, updated_at, user_action_type) VALUES
    ('2t4eb898-1410-40d3-8c1e-27c1087f4132', 'd5644e83-ffa3-4e0d-ae81-c3155eedb8fd', 'SPECIAL_ZONES', 'USER_FULL_ACCESS', now(), now(), 'SPECIAL_ZONE_CREATE');

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, created_at, updated_at, user_action_type) VALUES
    ('2t4eb898-1410-40d3-8c1e-27c1087f4133', 'd5644e83-ffa3-4e0d-ae81-c3155eedb8fd', 'SPECIAL_ZONES', 'USER_FULL_ACCESS', now(), now(),'SPECIAL_ZONE_LOOKUP');

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, created_at, updated_at, user_action_type) VALUES
    ('2t4eb898-1410-40d3-8c1e-27c1087f4134', 'd5644e83-ffa3-4e0d-ae81-c3155eedb8fd', 'SPECIAL_ZONES', 'USER_FULL_ACCESS', now(), now(),'SPECIAL_ZONE_UPDATE');

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, created_at, updated_at, user_action_type) VALUES
    ('2t4eb898-1410-40d3-8c1e-27c1087f4135', 'd5644e83-ffa3-4e0d-ae81-c3155eedb8fd', 'SPECIAL_ZONES', 'USER_FULL_ACCESS', now(), now(), 'SPECIAL_ZONE_DELETE');

INSERT INTO atlas_bpp_dashboard.registration_token (id, token, person_id, created_at, merchant_id) VALUES
    ('8k666614-c6f1-48a8-ab16-23873b93f452', '141e0a7b-e521-4d6b-a0c4-8713ae345154', 'a77b0507-ae01-42dd-a075-264f59d89049', now (), 'sz2db186-39d3-48a4-ad1f-78a0c3f840fd');

INSERT INTO atlas_bpp_dashboard.merchant_access(id, person_id, created_at, merchant_id)
	VALUES ('8k666614-c6f1-48a8-ab16-23267b93f452','a77b0507-ae01-42dd-a075-264f59d89049' , now(), 'sz2db186-39d3-48a4-ad1f-78a0c3f840fd' );