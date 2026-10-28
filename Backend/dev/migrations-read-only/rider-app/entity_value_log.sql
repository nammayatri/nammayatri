CREATE TABLE atlas_app.entity_value_log ();

ALTER TABLE atlas_app.entity_value_log ADD COLUMN actor_id text ;
ALTER TABLE atlas_app.entity_value_log ADD COLUMN actor_type text NOT NULL;
ALTER TABLE atlas_app.entity_value_log ADD COLUMN delta integer NOT NULL;
ALTER TABLE atlas_app.entity_value_log ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_app.entity_value_log ADD COLUMN entity_type text NOT NULL;
ALTER TABLE atlas_app.entity_value_log ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.entity_value_log ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.entity_value_log ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.entity_value_log ADD COLUMN operation_id text NOT NULL;
ALTER TABLE atlas_app.entity_value_log ADD COLUMN operation_type text NOT NULL;
ALTER TABLE atlas_app.entity_value_log ADD COLUMN reason text ;
ALTER TABLE atlas_app.entity_value_log ADD COLUMN value_after integer NOT NULL;
ALTER TABLE atlas_app.entity_value_log ADD COLUMN value_before integer NOT NULL;
ALTER TABLE atlas_app.entity_value_log ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.entity_value_log ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.entity_value_log ADD PRIMARY KEY ( id);
