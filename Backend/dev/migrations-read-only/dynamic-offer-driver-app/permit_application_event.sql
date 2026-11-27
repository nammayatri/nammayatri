CREATE TABLE atlas_driver_offer_bpp.permit_application_event ();

ALTER TABLE atlas_driver_offer_bpp.permit_application_event ADD COLUMN action text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_application_event ADD COLUMN actor_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_application_event ADD COLUMN actor_name text ;
ALTER TABLE atlas_driver_offer_bpp.permit_application_event ADD COLUMN actor_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_application_event ADD COLUMN application_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_application_event ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.permit_application_event ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.permit_application_event ADD COLUMN reason text ;
ALTER TABLE atlas_driver_offer_bpp.permit_application_event ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.permit_application_event ADD PRIMARY KEY ( id);
