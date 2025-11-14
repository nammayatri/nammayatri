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


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.volunteer ADD COLUMN is_active boolean  default true;
ALTER TABLE atlas_driver_offer_bpp.volunteer DROP CONSTRAINT volunteer_pkey;
ALTER TABLE atlas_driver_offer_bpp.volunteer ADD PRIMARY KEY ( id, vendor_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.volunteer ALTER COLUMN vendor_id SET DEFAULT 'DEFAULT_VENDOR';
ALTER TABLE atlas_driver_offer_bpp.volunteer ALTER COLUMN vendor_id SET NOT NULL;