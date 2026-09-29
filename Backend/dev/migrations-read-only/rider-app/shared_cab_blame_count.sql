CREATE TABLE atlas_app.shared_cab_blame_count ();

ALTER TABLE atlas_app.shared_cab_blame_count ADD COLUMN "count" integer NOT NULL default 0;
ALTER TABLE atlas_app.shared_cab_blame_count ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.shared_cab_blame_count ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.shared_cab_blame_count ADD COLUMN last_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.shared_cab_blame_count ADD COLUMN last_booking_id text NOT NULL;
ALTER TABLE atlas_app.shared_cab_blame_count ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.shared_cab_blame_count ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.shared_cab_blame_count ADD COLUMN subject_id text NOT NULL;
ALTER TABLE atlas_app.shared_cab_blame_count ADD COLUMN subject_type text NOT NULL;
ALTER TABLE atlas_app.shared_cab_blame_count ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.shared_cab_blame_count ADD PRIMARY KEY ( id);
CREATE INDEX CONCURRENTLY shared_cab_blame_count_idx_merchant_operating_city_id_subject_id_subject_type ON atlas_app.shared_cab_blame_count USING btree (merchant_operating_city_id, subject_id, subject_type);
CREATE INDEX CONCURRENTLY shared_cab_blame_count_idx_count_merchant_operating_city_id_subject_type ON atlas_app.shared_cab_blame_count USING btree ("count", merchant_operating_city_id, subject_type);