CREATE TABLE atlas_app.partner_invoice_data_log ();

ALTER TABLE atlas_app.partner_invoice_data_log ADD COLUMN booking_id character(36) NOT NULL;
ALTER TABLE atlas_app.partner_invoice_data_log ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.partner_invoice_data_log ADD COLUMN exported_at timestamp with time zone ;
ALTER TABLE atlas_app.partner_invoice_data_log ADD COLUMN id character(36) NOT NULL;
ALTER TABLE atlas_app.partner_invoice_data_log ADD COLUMN merchant_id character(36) NOT NULL;
ALTER TABLE atlas_app.partner_invoice_data_log ADD COLUMN merchant_operating_city_id character(36) NOT NULL;
ALTER TABLE atlas_app.partner_invoice_data_log ADD COLUMN person_id character(36) NOT NULL;
ALTER TABLE atlas_app.partner_invoice_data_log ADD COLUMN requested_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.partner_invoice_data_log ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.partner_invoice_data_log ADD PRIMARY KEY ( id);
