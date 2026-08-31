CREATE TABLE atlas_app.user_cohort_mapping ();

ALTER TABLE atlas_app.user_cohort_mapping ADD COLUMN cohort_mapping_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.user_cohort_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.user_cohort_mapping ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.user_cohort_mapping ADD COLUMN is_test_group boolean  default NULL;
ALTER TABLE atlas_app.user_cohort_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.user_cohort_mapping ADD COLUMN user_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.user_cohort_mapping ADD PRIMARY KEY ( id);
CREATE INDEX CONCURRENTLY user_cohort_mapping_idx_user_id ON atlas_app.user_cohort_mapping USING btree (user_id);
CREATE INDEX CONCURRENTLY user_cohort_mapping_idx_cohort_mapping_id ON atlas_app.user_cohort_mapping USING btree (cohort_mapping_id);
ALTER TABLE atlas_app.user_cohort_mapping ADD CONSTRAINT user_cohort_mapping_unique_idx_cohort_mapping_id_user_id UNIQUE (cohort_mapping_id, user_id);