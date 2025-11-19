CREATE TABLE atlas_driver_offer_bpp.volunteer ();

ALTER TABLE atlas_driver_offer_bpp.volunteer ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.volunteer ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.volunteer ADD COLUMN place text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.volunteer ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.volunteer ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.volunteer ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.volunteer ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.volunteer ADD COLUMN vendor_id text ;
ALTER TABLE atlas_driver_offer_bpp.volunteer ADD COLUMN is_active boolean  default true;