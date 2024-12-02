CREATE TABLE atlas_driver_offer_bpp.invoice ();

ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN bank_error_code text ;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN bank_error_message text ;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN bank_error_updated_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN driver_fee_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN invoice_short_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN invoice_status text NOT NULL default 'ACTIVE_INVOICE';
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN last_status_checked_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN max_mandate_amount integer ;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN payment_mode text NOT NULL default 'MANUAL_INVOICE';
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN service_name text  default 'YATRI_SUBSCRIPTION';
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN merchant_id character varying(36) ;