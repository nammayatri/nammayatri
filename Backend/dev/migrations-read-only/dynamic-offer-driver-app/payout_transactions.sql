CREATE TABLE atlas_driver_offer_bpp.payout_transactions ();

ALTER TABLE atlas_driver_offer_bpp.payout_transactions ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_transactions ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_transactions ADD COLUMN fulfillment_method text ;
ALTER TABLE atlas_driver_offer_bpp.payout_transactions ADD COLUMN gate_way_ref_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_transactions ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_transactions ADD COLUMN payout_order_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_transactions ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_transactions ADD COLUMN transaction_ref text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_transactions ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_transactions ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.payout_transactions ADD PRIMARY KEY ( id, transaction_ref);