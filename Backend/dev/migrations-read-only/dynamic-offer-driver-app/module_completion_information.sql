CREATE TABLE atlas_driver_offer_bpp.module_completion_information ();

ALTER TABLE atlas_driver_offer_bpp.module_completion_information ADD COLUMN attempt integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.module_completion_information ADD COLUMN completion_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.module_completion_information ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.module_completion_information ADD COLUMN entity text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.module_completion_information ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.module_completion_information ADD COLUMN entity_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.module_completion_information ADD COLUMN selected_entity_id text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.module_completion_information ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.module_completion_information ADD PRIMARY KEY ( attempt, completion_id, entity, entity_id);