CREATE TABLE atlas_driver_offer_bpp.user_cohort_mapping ();

ALTER TABLE atlas_driver_offer_bpp.user_cohort_mapping ADD COLUMN cohort_mapping_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.user_cohort_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.user_cohort_mapping ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.user_cohort_mapping ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.user_cohort_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.user_cohort_mapping ADD COLUMN user_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.user_cohort_mapping ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.user_cohort_mapping ADD PRIMARY KEY ( id);
CREATE INDEX CONCURRENTLY user_cohort_mapping_idx_user_id ON atlas_driver_offer_bpp.user_cohort_mapping USING btree (user_id);
CREATE INDEX CONCURRENTLY user_cohort_mapping_idx_cohort_mapping_id ON atlas_driver_offer_bpp.user_cohort_mapping USING btree (cohort_mapping_id);
ALTER TABLE atlas_driver_offer_bpp.user_cohort_mapping ADD CONSTRAINT user_cohort_mapping_unique_idx_cohort_mapping_id_user_id UNIQUE (cohort_mapping_id, user_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.user_cohort_mapping ADD COLUMN terms_accepted_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.user_cohort_mapping ALTER COLUMN terms_accepted_at SET DEFAULT NULL;
ALTER TABLE atlas_driver_offer_bpp.user_cohort_mapping ADD COLUMN opted_in_by text  default NULL;
ALTER TABLE atlas_driver_offer_bpp.user_cohort_mapping ADD COLUMN opted_in_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.user_cohort_mapping ALTER COLUMN opted_in_at SET DEFAULT NULL;
ALTER TABLE atlas_driver_offer_bpp.user_cohort_mapping ADD COLUMN opt_in_period text  default NULL;