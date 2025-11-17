CREATE TABLE atlas_driver_offer_bpp.vendor_settlement ();

ALTER TABLE atlas_driver_offer_bpp.vendor_settlement ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vendor_settlement ADD COLUMN from_vendor_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vendor_settlement ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vendor_settlement ADD COLUMN running_balance double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vendor_settlement ADD COLUMN settlement_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.vendor_settlement ADD COLUMN settlement_mode text NOT NULL default 'Manual';
ALTER TABLE atlas_driver_offer_bpp.vendor_settlement ADD COLUMN status text NOT NULL default 'Pending';
ALTER TABLE atlas_driver_offer_bpp.vendor_settlement ADD COLUMN to_vendor_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vendor_settlement ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vendor_settlement ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.vendor_settlement ADD PRIMARY KEY ( from_vendor_id, id, to_vendor_id);
