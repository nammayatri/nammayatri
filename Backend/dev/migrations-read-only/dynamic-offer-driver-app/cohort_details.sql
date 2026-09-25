CREATE TABLE atlas_driver_offer_bpp.cohort_details ();

ALTER TABLE atlas_driver_offer_bpp.cohort_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.cohort_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cohort_details ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cohort_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.cohort_details ADD PRIMARY KEY ( id);


ALTER TABLE atlas_driver_offer_bpp.cohort_details ADD COLUMN description text  default NULL;
ALTER TABLE atlas_driver_offer_bpp.cohort_details ADD COLUMN cohort_rule json  default NULL;
ALTER TABLE atlas_driver_offer_bpp.cohort_details ADD COLUMN category text  default NULL;

ALTER TABLE atlas_driver_offer_bpp.cohort_details ADD CONSTRAINT cohort_details_unique_idx_category_name UNIQUE (category, name);