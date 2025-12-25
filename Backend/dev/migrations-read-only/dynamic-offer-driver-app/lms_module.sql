CREATE TABLE atlas_driver_offer_bpp.lms_module ();

ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN category text NOT NULL default 'Training';
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN duration integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN languages_available_for_quiz text[] NOT NULL default '{}';
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN languages_available_for_videos text[] NOT NULL default '{}';
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN module_completion_criteria text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN no_of_videos integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN rank integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN variant text ;
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN module_section text  default 'BENEFITS';
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN module_name_for_certificate text ;
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN module_expiry_config integer ;
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN certification_enabled boolean ;
ALTER TABLE atlas_driver_offer_bpp.lms_module ADD COLUMN bonus_coin_event_function text ;