CREATE TABLE atlas_app.finance_current_state ();

ALTER TABLE atlas_app.finance_current_state ADD COLUMN current_state text NOT NULL;
ALTER TABLE atlas_app.finance_current_state ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_app.finance_current_state ADD COLUMN entity_type text NOT NULL;
ALTER TABLE atlas_app.finance_current_state ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.finance_current_state ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_app.finance_current_state ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.finance_current_state ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.finance_current_state ADD PRIMARY KEY ( entity_id);
