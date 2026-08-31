CREATE TABLE atlas_app.cohort_journey_mapping ();

ALTER TABLE atlas_app.cohort_journey_mapping ADD COLUMN cohort_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.cohort_journey_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.cohort_journey_mapping ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.cohort_journey_mapping ADD COLUMN journey_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.cohort_journey_mapping ADD COLUMN start_date timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.cohort_journey_mapping ADD COLUMN streak_end_reward_type text  default NULL;
ALTER TABLE atlas_app.cohort_journey_mapping ADD COLUMN streak_end_reward_value integer  default NULL;
ALTER TABLE atlas_app.cohort_journey_mapping ADD COLUMN streak_range integer NOT NULL;
ALTER TABLE atlas_app.cohort_journey_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.cohort_journey_mapping ADD PRIMARY KEY ( id);
CREATE INDEX CONCURRENTLY cohort_journey_mapping_idx_cohort_id ON atlas_app.cohort_journey_mapping USING btree (cohort_id);
CREATE INDEX CONCURRENTLY cohort_journey_mapping_idx_journey_id ON atlas_app.cohort_journey_mapping USING btree (journey_id);
ALTER TABLE atlas_app.cohort_journey_mapping ADD CONSTRAINT cohort_journey_mapping_unique_idx_cohort_id_journey_id UNIQUE (cohort_id, journey_id);


------- SQL updates -------

ALTER TABLE atlas_app.cohort_journey_mapping ADD COLUMN streak_end_reward_expiration_at integer  default NULL;