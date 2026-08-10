-- ============================================================================
-- Load both exports into atlas_dashboard as prefixed staging tables.
-- RUN AGAINST THE **atlas_dashboard** DATABASE, from the same directory:
--     cd /tmp/unify && psql atlas_dashboard -f <this file>
--
-- Staging tables are cloned from the real merged tables (which already exist in
-- this schema), so column types line up and no DDL is hand-maintained. They are
-- read by the merge and dropped by 0012.
-- ============================================================================
\set ON_ERROR_STOP on
BEGIN;
DROP TABLE IF EXISTS atlas_dashboard.bap_merchant_operating_city;
CREATE TABLE atlas_dashboard.bap_merchant_operating_city (LIKE atlas_dashboard.merchant_operating_city INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bap_role;
CREATE TABLE atlas_dashboard.bap_role (LIKE atlas_dashboard.role INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bap_person;
CREATE TABLE atlas_dashboard.bap_person (LIKE atlas_dashboard.person INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bap_merchant;
CREATE TABLE atlas_dashboard.bap_merchant (LIKE atlas_dashboard.merchant INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bap_merchant_access;
CREATE TABLE atlas_dashboard.bap_merchant_access (LIKE atlas_dashboard.merchant_access INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bap_access_matrix;
CREATE TABLE atlas_dashboard.bap_access_matrix (LIKE atlas_dashboard.access_matrix INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bap_entity;
CREATE TABLE atlas_dashboard.bap_entity (LIKE atlas_dashboard.entity INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bap_transaction;
CREATE TABLE atlas_dashboard.bap_transaction (LIKE atlas_dashboard.transaction INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bap_person_capability;
CREATE TABLE atlas_dashboard.bap_person_capability (LIKE atlas_dashboard.person_capability INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bap_access_audit;
CREATE TABLE atlas_dashboard.bap_access_audit (LIKE atlas_dashboard.access_audit INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bpp_merchant_operating_city;
CREATE TABLE atlas_dashboard.bpp_merchant_operating_city (LIKE atlas_dashboard.merchant_operating_city INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bpp_role;
CREATE TABLE atlas_dashboard.bpp_role (LIKE atlas_dashboard.role INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bpp_person;
CREATE TABLE atlas_dashboard.bpp_person (LIKE atlas_dashboard.person INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bpp_merchant;
CREATE TABLE atlas_dashboard.bpp_merchant (LIKE atlas_dashboard.merchant INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bpp_merchant_access;
CREATE TABLE atlas_dashboard.bpp_merchant_access (LIKE atlas_dashboard.merchant_access INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bpp_access_matrix;
CREATE TABLE atlas_dashboard.bpp_access_matrix (LIKE atlas_dashboard.access_matrix INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bpp_entity;
CREATE TABLE atlas_dashboard.bpp_entity (LIKE atlas_dashboard.entity INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bpp_transaction;
CREATE TABLE atlas_dashboard.bpp_transaction (LIKE atlas_dashboard.transaction INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bpp_person_capability;
CREATE TABLE atlas_dashboard.bpp_person_capability (LIKE atlas_dashboard.person_capability INCLUDING DEFAULTS);
DROP TABLE IF EXISTS atlas_dashboard.bpp_access_audit;
CREATE TABLE atlas_dashboard.bpp_access_audit (LIKE atlas_dashboard.access_audit INCLUDING DEFAULTS);
-- Staging tables are scratch: the merge only reads them. Relax NOT NULL on every
-- column so a column missing from an export (e.g. admin_tier, added after these
-- rows were written) cannot fail the COPY. Types and defaults are still the
-- real ones, so values that ARE present land correctly.
DO $$
DECLARE r record;
BEGIN
  FOR r IN
    SELECT c.relname, a.attname
    FROM pg_class c
    JOIN pg_namespace n ON n.oid = c.relnamespace AND n.nspname = 'atlas_dashboard'
    JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum > 0 AND NOT a.attisdropped
    WHERE (c.relname LIKE 'bap\_%' OR c.relname LIKE 'bpp\_%')
      AND c.relkind = 'r' AND a.attnotnull
  LOOP
    EXECUTE format('ALTER TABLE atlas_dashboard.%I ALTER COLUMN %I DROP NOT NULL',
                   r.relname, r.attname);
  END LOOP;
END $$;

COMMIT;

\copy atlas_dashboard.bap_merchant_operating_city (id, city, std_code) FROM 'bap_merchant_operating_city.csv' WITH (FORMAT csv, HEADER true)
\copy atlas_dashboard.bap_role (id, name, dashboard_access_type, description, created_at, updated_at, accessible_roles) FROM 'bap_role.csv' WITH (FORMAT csv, HEADER true)
\copy atlas_dashboard.bap_person (id, first_name, last_name, email_encrypted, email_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, password_hash, created_at, updated_at, role_id, dashboard_access_type, verified, receive_notification, rejection_reason, rejected_at, dashboard_type, password_updated_at, approved_by, rejected_by, language, secret_key, is2fa_enabled, token_no_hash, entity_id) FROM 'bap_person.csv' WITH (FORMAT csv, HEADER true)
\copy atlas_dashboard.bap_merchant (id, short_id, server_name, created_at, default_operating_city, supported_operating_cities, server_names, domain, website, enabled, auth_token_encrypted, auth_token_hash, require_admin_approval_for_fleet_onboarding, has_fleet_member_hierarchy, is_strong_name_check_required, verify_fleet_while_login, single_active_session_only, track_login_logout_for_roles, two_factor_mandatory_for_roles) FROM 'bap_merchant.csv' WITH (FORMAT csv, HEADER true)
\copy atlas_dashboard.bap_merchant_access (id, person_id, created_at, merchant_id, merchant_short_id, operating_city) FROM 'bap_merchant_access.csv' WITH (FORMAT csv, HEADER true)
\copy atlas_dashboard.bap_access_matrix (id, role_id, api_entity, user_access_type, created_at, updated_at, user_action_type) FROM 'bap_access_matrix.csv' WITH (FORMAT csv, HEADER true)
-- optional, only if exported:
-- \copy atlas_dashboard.bap_entity (id, merchant_id, entity_name, entity_short_id, deleted, created_at, updated_at) FROM 'bap_entity.csv' WITH (FORMAT csv, HEADER true)
-- optional, only if exported:
-- \copy atlas_dashboard.bap_transaction (id, requestor_id, server_name, merchant_id, endpoint, common_driver_id, common_ride_id, request, response, response_error, created_at) FROM 'bap_transaction.csv' WITH (FORMAT csv, HEADER true)
\copy atlas_dashboard.bap_person_capability (person_id, capability_id, mode, reason, granted_by, expires_at, created_at) FROM 'bap_person_capability.csv' WITH (FORMAT csv, HEADER true)
\copy atlas_dashboard.bap_access_audit (id, actor_id, action, target_type, target_id, before_value, after_value, reason, created_at) FROM 'bap_access_audit.csv' WITH (FORMAT csv, HEADER true)
\copy atlas_dashboard.bpp_merchant_operating_city (id, city, std_code) FROM 'bpp_merchant_operating_city.csv' WITH (FORMAT csv, HEADER true)
\copy atlas_dashboard.bpp_role (id, name, dashboard_access_type, description, created_at, updated_at, accessible_roles) FROM 'bpp_role.csv' WITH (FORMAT csv, HEADER true)
\copy atlas_dashboard.bpp_person (id, first_name, last_name, email_encrypted, email_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, password_hash, created_at, updated_at, role_id, dashboard_access_type, verified, receive_notification, rejection_reason, rejected_at, dashboard_type, password_updated_at, approved_by, rejected_by, language, secret_key, is2fa_enabled, token_no_hash, entity_id) FROM 'bpp_person.csv' WITH (FORMAT csv, HEADER true)
\copy atlas_dashboard.bpp_merchant (id, short_id, server_name, created_at, default_operating_city, supported_operating_cities, server_names, company_name, domain, website, email_hash, password_hash, email_encrypted, enabled, auth_token_encrypted, auth_token_hash, require_admin_approval_for_fleet_onboarding, has_fleet_member_hierarchy, is_strong_name_check_required, verify_fleet_while_login, single_active_session_only, track_login_logout_for_roles, two_factor_mandatory_for_roles) FROM 'bpp_merchant.csv' WITH (FORMAT csv, HEADER true)
\copy atlas_dashboard.bpp_merchant_access (id, person_id, created_at, merchant_id, merchant_short_id, operating_city) FROM 'bpp_merchant_access.csv' WITH (FORMAT csv, HEADER true)
\copy atlas_dashboard.bpp_access_matrix (id, role_id, api_entity, user_access_type, created_at, updated_at, user_action_type) FROM 'bpp_access_matrix.csv' WITH (FORMAT csv, HEADER true)
-- optional, only if exported:
-- \copy atlas_dashboard.bpp_entity (id, merchant_id, entity_name, entity_short_id, deleted, created_at, updated_at) FROM 'bpp_entity.csv' WITH (FORMAT csv, HEADER true)
-- optional, only if exported:
-- \copy atlas_dashboard.bpp_transaction (id, requestor_id, server_name, merchant_id, endpoint, common_driver_id, common_ride_id, request, response, response_error, created_at) FROM 'bpp_transaction.csv' WITH (FORMAT csv, HEADER true)
\copy atlas_dashboard.bpp_person_capability (person_id, capability_id, mode, reason, granted_by, expires_at, created_at) FROM 'bpp_person_capability.csv' WITH (FORMAT csv, HEADER true)
\copy atlas_dashboard.bpp_access_audit (id, actor_id, action, target_type, target_id, before_value, after_value, reason, created_at) FROM 'bpp_access_audit.csv' WITH (FORMAT csv, HEADER true)

-- Reconcile these against the source counts before running the merge.
SELECT 'bap' side, 'role' t, count(*) FROM atlas_dashboard.bap_role
UNION ALL SELECT 'bap','person',          count(*) FROM atlas_dashboard.bap_person
UNION ALL SELECT 'bap','merchant',        count(*) FROM atlas_dashboard.bap_merchant
UNION ALL SELECT 'bap','merchant_access', count(*) FROM atlas_dashboard.bap_merchant_access
UNION ALL SELECT 'bap','access_matrix',   count(*) FROM atlas_dashboard.bap_access_matrix
UNION ALL SELECT 'bpp','role',            count(*) FROM atlas_dashboard.bpp_role
UNION ALL SELECT 'bpp','person',          count(*) FROM atlas_dashboard.bpp_person
UNION ALL SELECT 'bpp','merchant',        count(*) FROM atlas_dashboard.bpp_merchant
UNION ALL SELECT 'bpp','merchant_access', count(*) FROM atlas_dashboard.bpp_merchant_access
UNION ALL SELECT 'bpp','access_matrix',   count(*) FROM atlas_dashboard.bpp_access_matrix;
