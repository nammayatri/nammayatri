CREATE TABLE atlas_driver_offer_bpp.invoice_template ();

ALTER TABLE atlas_driver_offer_bpp.invoice_template ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.invoice_template ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.invoice_template ADD COLUMN invoice_type text ;
ALTER TABLE atlas_driver_offer_bpp.invoice_template ADD COLUMN language text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.invoice_template ADD COLUMN line_item_row_template text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.invoice_template ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.invoice_template ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.invoice_template ADD COLUMN template text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.invoice_template ADD COLUMN totals_line_row_template text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.invoice_template ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.invoice_template ADD PRIMARY KEY ( id);
