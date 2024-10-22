CREATE TABLE atlas_driver_offer_bpp.llm_prompt ();

ALTER TABLE atlas_driver_offer_bpp.llm_prompt ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.llm_prompt ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.llm_prompt ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.llm_prompt ADD COLUMN prompt_key character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.llm_prompt ADD COLUMN prompt_template text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.llm_prompt ADD COLUMN service_name character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.llm_prompt ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.llm_prompt ADD COLUMN use_case character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.llm_prompt ADD PRIMARY KEY ( merchant_operating_city_id, prompt_key, service_name, use_case);
