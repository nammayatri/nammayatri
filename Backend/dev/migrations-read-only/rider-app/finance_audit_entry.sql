CREATE TABLE atlas_app.finance_audit_entry ();

ALTER TABLE atlas_app.finance_audit_entry ADD COLUMN action text NOT NULL;
ALTER TABLE atlas_app.finance_audit_entry ADD COLUMN actor_id text ;
ALTER TABLE atlas_app.finance_audit_entry ADD COLUMN actor_type text NOT NULL;
ALTER TABLE atlas_app.finance_audit_entry ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.finance_audit_entry ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_app.finance_audit_entry ADD COLUMN entity_type text NOT NULL;
ALTER TABLE atlas_app.finance_audit_entry ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.finance_audit_entry ADD COLUMN ip_address text ;
ALTER TABLE atlas_app.finance_audit_entry ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.finance_audit_entry ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_app.finance_audit_entry ADD COLUMN metadata text ;
ALTER TABLE atlas_app.finance_audit_entry ADD COLUMN new_state text ;
ALTER TABLE atlas_app.finance_audit_entry ADD COLUMN previous_state text ;
ALTER TABLE atlas_app.finance_audit_entry ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.finance_audit_entry ADD PRIMARY KEY ( id);
