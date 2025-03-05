CREATE TABLE atlas_driver_offer_bpp.voip_call_status ();

ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN call_id text ;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN call_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN error_code integer ;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN merchant_city text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN network_quality text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN network_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN ride_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.voip_call_status ADD PRIMARY KEY ( id);
CREATE INDEX idx_voip_call_status_call_id ON atlas_driver_offer_bpp.voip_call_status USING btree (call_id);
