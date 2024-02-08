CREATE TABLE atlas_driver_offer_bpp.lms_module_video_information ();

ALTER TABLE atlas_driver_offer_bpp.lms_module_video_information ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.lms_module_video_information ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_module_video_information ADD COLUMN module_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_module_video_information ADD COLUMN rank integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_module_video_information ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.lms_module_video_information ADD COLUMN video_status text NOT NULL default 'ACTIVE';
ALTER TABLE atlas_driver_offer_bpp.lms_module_video_information ADD PRIMARY KEY ( id);