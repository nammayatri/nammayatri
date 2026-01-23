CREATE TABLE atlas_driver_offer_bpp.payout_status_history ();

ALTER TABLE atlas_driver_offer_bpp.payout_status_history ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_status_history ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_status_history ADD COLUMN message text ;
ALTER TABLE atlas_driver_offer_bpp.payout_status_history ADD COLUMN scheduled_payout_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_status_history ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_status_history ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.payout_status_history ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.payout_status_history ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_status_history ADD PRIMARY KEY ( id);
