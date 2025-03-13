CREATE TABLE atlas_driver_offer_bpp.operation_hub ();

ALTER TABLE atlas_driver_offer_bpp.operation_hub ADD COLUMN address text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub ADD COLUMN description text ;
ALTER TABLE atlas_driver_offer_bpp.operation_hub ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub ADD COLUMN lat double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub ADD COLUMN lon double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub ADD COLUMN mobile_number text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.operation_hub ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.operation_hub ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.operation_hub ADD PRIMARY KEY ( id);
