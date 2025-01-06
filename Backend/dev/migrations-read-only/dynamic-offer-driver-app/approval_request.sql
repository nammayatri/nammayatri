CREATE TABLE atlas_driver_offer_bpp.approval_request ();

ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN body text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN reason text ;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN request_data text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN requestee_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN requestor_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN title text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN trip_transaction_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD PRIMARY KEY ( id);



------- SQL updates -------


--- Now DSL don't allow dropping tables instead we will drop not null constraint if any .Please be careful while running ---
ALTER TABLE atlas_driver_offer_bpp.approval_request ALTER COLUMN trip_transaction_id DROP NOT NULL;
--- Drop section ends. Please check before running ---



------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

