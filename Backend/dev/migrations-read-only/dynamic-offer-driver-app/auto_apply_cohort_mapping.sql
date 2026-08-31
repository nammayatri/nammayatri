CREATE TABLE atlas_driver_offer_bpp.auto_apply_cohort_mapping ();

ALTER TABLE atlas_driver_offer_bpp.auto_apply_cohort_mapping ADD COLUMN allow_if_no_mapping boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.auto_apply_cohort_mapping ADD COLUMN cohort_journey_mapping_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.auto_apply_cohort_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.auto_apply_cohort_mapping ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.auto_apply_cohort_mapping ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.auto_apply_cohort_mapping ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.auto_apply_cohort_mapping ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.auto_apply_cohort_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.auto_apply_cohort_mapping ADD COLUMN vehicle_category text ;
ALTER TABLE atlas_driver_offer_bpp.auto_apply_cohort_mapping ADD PRIMARY KEY ( id);
CREATE INDEX CONCURRENTLY auto_apply_cohort_mapping_idx_merchant_id_merchant_operating_city_id_vehicle_category ON atlas_driver_offer_bpp.auto_apply_cohort_mapping USING btree (merchant_id, merchant_operating_city_id, vehicle_category);
ALTER TABLE atlas_driver_offer_bpp.auto_apply_cohort_mapping ADD CONSTRAINT auto_apply_cohort_mapping_unique_idx_cohort_journey_mapping_id_vehicle_category UNIQUE (cohort_journey_mapping_id, vehicle_category);