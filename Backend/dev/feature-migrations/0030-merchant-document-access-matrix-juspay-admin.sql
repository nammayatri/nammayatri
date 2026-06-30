-- Grant JUSPAY_ADMIN (role_id 37947162-3b5d-4ed6-bcac-08841be1534d) full access to
-- the MerchantDocument CRUD dashboard APIs.
-- Without these entries the provider-dashboard returns 403 ACCESS_DENIED for all
-- Dashboard: Create/List/Update/Delete MerchantDocument steps in the integration test.

INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at)
VALUES
  (gen_random_uuid()::text, '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_MERCHANT_DOCUMENT_LIST',   now(), now()),
  (gen_random_uuid()::text, '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_DOCUMENT_CREATE', now(), now()),
  (gen_random_uuid()::text, '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_DOCUMENT_UPDATE', now(), now()),
  (gen_random_uuid()::text, '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_DOCUMENT_DELETE', now(), now())
ON CONFLICT DO NOTHING;
