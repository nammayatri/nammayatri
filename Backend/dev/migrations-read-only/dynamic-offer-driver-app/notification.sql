CREATE TABLE atlas_driver_offer_bpp.notification ();

ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN date_created timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN driver_fee_id character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN juspay_provided_id character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN last_status_checked_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN last_updated timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN mandate_id character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN notification_type text ;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN provider_name text ;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN response_code text ;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN response_message text ;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN short_id character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN source_amount numeric NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN txn_date timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.notification ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN merchant_id character varying(36) ;