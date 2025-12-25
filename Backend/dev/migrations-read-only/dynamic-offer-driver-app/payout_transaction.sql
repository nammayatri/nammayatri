CREATE TABLE atlas_driver_offer_bpp.payout_transaction ();

ALTER TABLE atlas_driver_offer_bpp.payout_transaction ADD COLUMN currency text ;
ALTER TABLE atlas_driver_offer_bpp.payout_transaction ADD COLUMN price double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_transaction ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_transaction ADD COLUMN fulfillment_method text ;
ALTER TABLE atlas_driver_offer_bpp.payout_transaction ADD COLUMN gate_way_ref_id text ;
ALTER TABLE atlas_driver_offer_bpp.payout_transaction ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_transaction ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_transaction ADD COLUMN payout_order_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_transaction ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_transaction ADD COLUMN transaction_ref text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_transaction ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_transaction ADD PRIMARY KEY ( id, transaction_ref);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.payout_transaction ADD COLUMN merchant_operating_city_id text ;