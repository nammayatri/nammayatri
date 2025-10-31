CREATE TABLE atlas_driver_offer_bpp.vendor_fee ();

ALTER TABLE atlas_driver_offer_bpp.vendor_fee ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vendor_fee ADD COLUMN driver_fee_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vendor_fee ADD COLUMN vendor_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vendor_fee ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vendor_fee ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vendor_fee ADD PRIMARY KEY ( driver_fee_id, vendor_id);

CREATE INDEX idx_driver_fee ON atlas_driver_offer_bpp.vendor_fee USING btree (driver_fee_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vendor_fee ADD COLUMN split_method text  default 'Normal';
ALTER TABLE atlas_driver_offer_bpp.vendor_fee ADD COLUMN is_vendor_fee_processed_at timestamp with time zone ;
DROP INDEX idx_driver_fee;