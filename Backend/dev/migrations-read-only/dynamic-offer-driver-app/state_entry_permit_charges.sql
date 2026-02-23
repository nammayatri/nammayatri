CREATE TABLE atlas_driver_offer_bpp.state_entry_permit_charges ();

ALTER TABLE atlas_driver_offer_bpp.state_entry_permit_charges ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.state_entry_permit_charges ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.state_entry_permit_charges ADD COLUMN geom_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.state_entry_permit_charges ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.state_entry_permit_charges ADD COLUMN name text ;
ALTER TABLE atlas_driver_offer_bpp.state_entry_permit_charges ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.state_entry_permit_charges ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.state_entry_permit_charges ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.state_entry_permit_charges ADD PRIMARY KEY ( id);
