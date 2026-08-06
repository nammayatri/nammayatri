CREATE TABLE atlas_driver_offer_bpp.rsf_order_state ();

ALTER TABLE atlas_driver_offer_bpp.rsf_order_state ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.rsf_order_state ADD COLUMN last_reported_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.rsf_order_state ADD COLUMN last_reported_message_id text ;
ALTER TABLE atlas_driver_offer_bpp.rsf_order_state ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rsf_order_state ADD COLUMN order_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rsf_order_state ADD COLUMN reported_code text ;
ALTER TABLE atlas_driver_offer_bpp.rsf_order_state ADD COLUMN reported_diff numeric(30,2) ;
ALTER TABLE atlas_driver_offer_bpp.rsf_order_state ADD COLUMN reported_status text ;
ALTER TABLE atlas_driver_offer_bpp.rsf_order_state ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.rsf_order_state ADD PRIMARY KEY ( merchant_id, order_id);
