CREATE TABLE atlas_driver_offer_bpp.driver_ride_payout_bank_account ();

ALTER TABLE atlas_driver_offer_bpp.driver_ride_payout_bank_account ADD COLUMN bank_account_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_ride_payout_bank_account ADD COLUMN bank_account_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_ride_payout_bank_account ADD COLUMN bank_ifsc_code_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_ride_payout_bank_account ADD COLUMN bank_ifsc_code_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_ride_payout_bank_account ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_ride_payout_bank_account ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_ride_payout_bank_account ADD COLUMN rc_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_ride_payout_bank_account ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_ride_payout_bank_account ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_ride_payout_bank_account ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_ride_payout_bank_account ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_ride_payout_bank_account ADD PRIMARY KEY ( id);
