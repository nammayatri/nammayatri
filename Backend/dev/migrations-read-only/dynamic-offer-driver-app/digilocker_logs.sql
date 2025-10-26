CREATE TABLE atlas_driver_offer_bpp.digilocker_logs ();

ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD COLUMN digi_locker_state text ;
ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD COLUMN digi_locker_uri text ;
ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD COLUMN doc_type text ;
ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD COLUMN error_code text ;
ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD COLUMN error_description text ;
ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD COLUMN flow_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD COLUMN request_payload text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD COLUMN response_payload text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.digilocker_logs ADD PRIMARY KEY ( id);
