CREATE TABLE atlas_driver_offer_bpp.question_module_mapping ();

ALTER TABLE atlas_driver_offer_bpp.question_module_mapping ADD COLUMN module_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.question_module_mapping ADD COLUMN question_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.question_module_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.question_module_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.question_module_mapping ADD PRIMARY KEY ( module_id, question_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.question_module_mapping ADD COLUMN quiz_coin_function text ;