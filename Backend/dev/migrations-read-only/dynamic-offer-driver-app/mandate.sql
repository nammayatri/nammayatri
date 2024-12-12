CREATE TABLE atlas_driver_offer_bpp.mandate ();

ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN end_date timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN mandate_payment_flow text ;
ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN max_amount integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN payer_app text ;
ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN payer_app_name text ;
ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN payer_vpa text ;
ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN start_date timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.mandate ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN merchant_operating_city_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN merchant_id character varying(36) ;