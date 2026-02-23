CREATE TABLE atlas_driver_offer_bpp.coin_purchase_history ();

ALTER TABLE atlas_driver_offer_bpp.coin_purchase_history ADD COLUMN cash double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.coin_purchase_history ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.coin_purchase_history ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.coin_purchase_history ADD COLUMN driver_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.coin_purchase_history ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.coin_purchase_history ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.coin_purchase_history ADD COLUMN merchant_opt_city_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.coin_purchase_history ADD COLUMN num_coins integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.coin_purchase_history ADD COLUMN title text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.coin_purchase_history ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.coin_purchase_history ADD COLUMN vehicle_category text ;
ALTER TABLE atlas_driver_offer_bpp.coin_purchase_history ADD PRIMARY KEY ( id);




---- DO NOT RUN ABOVE QUERIES. ONLY RUN QUERIES BELOW THIS POINT. ------------------------------------------------------------------------------------------------------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.coin_purchase_history ADD COLUMN coin_redemption_type text  default 'SubscriptionUse';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.coin_purchase_history ADD COLUMN payout_order_id_for_direct_payout character varying(36) ;