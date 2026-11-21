ALTER TABLE atlas_app.igm_config ADD COLUMN IF NOT EXISTS respondent_name text;
ALTER TABLE atlas_app.igm_config ADD COLUMN IF NOT EXISTS respondent_phone text;
ALTER TABLE atlas_app.igm_config ADD COLUMN IF NOT EXISTS respondent_email text;
ALTER TABLE atlas_app.igm_config ADD COLUMN IF NOT EXISTS resolution_provider_name text;
ALTER TABLE atlas_app.igm_config ADD COLUMN IF NOT EXISTS resolution_provider_phone text;
ALTER TABLE atlas_app.igm_config ADD COLUMN IF NOT EXISTS resolution_provider_email text;
ALTER TABLE atlas_app.igm_config ADD COLUMN IF NOT EXISTS create_ticket_on_issue_raise boolean DEFAULT true;
ALTER TABLE atlas_app.issue_config ADD COLUMN IF NOT EXISTS should_sync_status_with_ticket_service boolean NOT NULL DEFAULT false;
ALTER TABLE atlas_app.issue_report ADD COLUMN IF NOT EXISTS issue_flow_type text;
