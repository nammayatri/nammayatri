-- ============================================================================
-- Export the BPP dashboard tables to CSV.
-- RUN AGAINST THE **atlas_bpp_dashboard** DATABASE, from the directory the files go in:
--     cd /tmp/unify && psql atlas_bpp_dashboard -f <this file>
-- psql does not interpolate variables inside \copy, so paths are literal.
-- ============================================================================
\set ON_ERROR_STOP on
\copy (SELECT id, name, dashboard_access_type, description, created_at, updated_at, accessible_roles FROM role) TO 'bpp_role.csv' WITH (FORMAT csv, HEADER true)
\copy (SELECT id, first_name, last_name, email_encrypted, email_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, password_hash, created_at, updated_at, role_id, dashboard_access_type, verified, receive_notification, rejection_reason, rejected_at, dashboard_type, password_updated_at, approved_by, rejected_by, language, secret_key, is2fa_enabled, token_no_hash, entity_id FROM person) TO 'bpp_person.csv' WITH (FORMAT csv, HEADER true)
\copy (SELECT id, city, std_code FROM merchant_operating_city) TO 'bpp_merchant_operating_city.csv' WITH (FORMAT csv, HEADER true)
\copy (SELECT id, short_id, server_name, created_at, default_operating_city, supported_operating_cities, server_names, company_name, domain, website, email_hash, password_hash, email_encrypted, enabled, auth_token_encrypted, auth_token_hash, require_admin_approval_for_fleet_onboarding, has_fleet_member_hierarchy, is_strong_name_check_required, verify_fleet_while_login, single_active_session_only, track_login_logout_for_roles, two_factor_mandatory_for_roles FROM merchant) TO 'bpp_merchant.csv' WITH (FORMAT csv, HEADER true)
\copy (SELECT id, person_id, created_at, merchant_id, merchant_short_id, operating_city FROM merchant_access) TO 'bpp_merchant_access.csv' WITH (FORMAT csv, HEADER true)
\copy (SELECT id, role_id, api_entity, user_access_type, created_at, updated_at, user_action_type FROM access_matrix) TO 'bpp_access_matrix.csv' WITH (FORMAT csv, HEADER true)
-- optional (comment out if this side has no `entity` table):
\copy (SELECT id, merchant_id, entity_name, entity_short_id, deleted, created_at, updated_at FROM entity) TO 'bpp_entity.csv' WITH (FORMAT csv, HEADER true)
-- optional (comment out if this side has no `transaction` table):
\copy (SELECT id, requestor_id, server_name, merchant_id, endpoint, common_driver_id, common_ride_id, request, response, response_error, created_at FROM transaction) TO 'bpp_transaction.csv' WITH (FORMAT csv, HEADER true)
\copy (SELECT person_id, capability_id, mode, reason, granted_by, expires_at, created_at FROM person_capability) TO 'bpp_person_capability.csv' WITH (FORMAT csv, HEADER true)
\copy (SELECT id, actor_id, action, target_type, target_id, before_value, after_value, reason, created_at FROM access_audit) TO 'bpp_access_audit.csv' WITH (FORMAT csv, HEADER true)
