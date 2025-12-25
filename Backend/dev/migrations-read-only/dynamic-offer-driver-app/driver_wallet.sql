CREATE TABLE atlas_driver_offer_bpp.driver_wallet ();

ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN driver_payable numeric(30,2) ;
ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN gst_deduction numeric(30,2) ;
ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN merchant_payable numeric(30,2) ;
ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN ride_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN running_balance numeric(30,2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN transaction_type character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN collection_mode text ;


ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN payout_order_id character varying(36) ;


ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN payout_status text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_wallet ADD COLUMN collection_amount numeric(30,2) ;
