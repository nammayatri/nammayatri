CREATE TABLE atlas_app.fare_breakup_info ();

ALTER TABLE atlas_app.fare_breakup_info ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_app.fare_breakup_info ADD COLUMN entity_type text NOT NULL;
ALTER TABLE atlas_app.fare_breakup_info ADD COLUMN fare_breakups text NOT NULL;
ALTER TABLE atlas_app.fare_breakup_info ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.fare_breakup_info ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.fare_breakup_info ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.fare_breakup_info ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.fare_breakup_info ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.fare_breakup_info ADD PRIMARY KEY ( id);
CREATE INDEX CONCURRENTLY fare_breakup_info_idx_entity_id ON atlas_app.fare_breakup_info USING btree (entity_id);
ALTER TABLE atlas_app.fare_breakup_info ADD CONSTRAINT fare_breakup_info_unique_idx_entity_id_entity_type UNIQUE (entity_id, entity_type);