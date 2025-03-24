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
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN trip_transaction_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN requestor_type text ;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN requestee_type text ;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN request_type text ;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN entity_type text ;
ALTER TABLE atlas_driver_offer_bpp.approval_request ADD COLUMN entity_id text ;




------- SQL updates -------

