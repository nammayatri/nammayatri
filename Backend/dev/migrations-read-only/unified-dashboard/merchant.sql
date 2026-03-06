CREATE TABLE atlas_dashboard.merchant ();

ALTER TABLE atlas_dashboard.merchant ADD COLUMN auth_token_encrypted text ;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN auth_token_hash text ;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN default_operating_city text NOT NULL;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN domain text ;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN enabled boolean ;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN has_fleet_member_hierarchy boolean ;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN is2fa_mandatory boolean NOT NULL;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN is_strong_name_check_required boolean ;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN require_admin_approval_for_fleet_onboarding boolean ;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN server_names text[] NOT NULL;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN single_active_session_only boolean ;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN supported_operating_cities text[] NOT NULL;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN verify_fleet_while_login boolean ;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN website text ;
ALTER TABLE atlas_dashboard.merchant ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_dashboard.merchant ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_dashboard.merchant ADD COLUMN enable_get_request_audit_logs boolean ;