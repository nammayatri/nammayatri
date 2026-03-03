-- {"api":"PostDomainDiscountConfigCreate","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DOMAIN_DISCOUNT_CONFIG/POST_DOMAIN_DISCOUNT_CONFIG_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDomainDiscountConfigList","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DOMAIN_DISCOUNT_CONFIG/GET_DOMAIN_DISCOUNT_CONFIG_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"DeleteDomainDiscountConfigDelete","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bpp_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DOMAIN_DISCOUNT_CONFIG/DELETE_DOMAIN_DISCOUNT_CONFIG_DELETE' ) ON CONFLICT DO NOTHING;
