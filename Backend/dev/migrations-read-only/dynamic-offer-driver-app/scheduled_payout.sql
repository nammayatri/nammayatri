CREATE TABLE atlas_driver_offer_bpp.scheduled_payout ();

ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD COLUMN amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD COLUMN booking_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD COLUMN driver_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD COLUMN ride_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD COLUMN retry_count integer ;
ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD COLUMN cancel_reason text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD COLUMN payout_transaction_id text ;
ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD COLUMN failure_reason text ;
ALTER TABLE atlas_driver_offer_bpp.scheduled_payout ADD COLUMN expected_credit_time timestamp with time zone ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

