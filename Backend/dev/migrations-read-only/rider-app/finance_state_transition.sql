CREATE TABLE atlas_app.finance_state_transition ();

ALTER TABLE atlas_app.finance_state_transition ADD COLUMN actor_id text ;
ALTER TABLE atlas_app.finance_state_transition ADD COLUMN actor_type text NOT NULL;
ALTER TABLE atlas_app.finance_state_transition ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_app.finance_state_transition ADD COLUMN entity_type text NOT NULL;
ALTER TABLE atlas_app.finance_state_transition ADD COLUMN event text NOT NULL;
ALTER TABLE atlas_app.finance_state_transition ADD COLUMN event_data text ;
ALTER TABLE atlas_app.finance_state_transition ADD COLUMN from_state text NOT NULL;
ALTER TABLE atlas_app.finance_state_transition ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.finance_state_transition ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.finance_state_transition ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_app.finance_state_transition ADD COLUMN to_state text NOT NULL;
ALTER TABLE atlas_app.finance_state_transition ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.finance_state_transition ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.finance_state_transition ADD PRIMARY KEY ( id);
