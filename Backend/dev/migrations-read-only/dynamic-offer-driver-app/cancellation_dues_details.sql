CREATE TABLE atlas_driver_offer_bpp.cancellation_dues_details ();

ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN cancellation_amount integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN payment_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN ride_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN merchant_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN cancellation_fee double precision ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN cancellation_fee_tax double precision ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN overdue_cancellation_charge double precision ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN overdue_cancellation_tax double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN overdue_cancellation_commission double precision ;
ALTER TABLE atlas_driver_offer_bpp.cancellation_dues_details ADD COLUMN cancellation_commission double precision ;