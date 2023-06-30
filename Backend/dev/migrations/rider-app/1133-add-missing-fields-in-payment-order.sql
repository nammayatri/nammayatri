ALTER TABLE atlas_app.payment_order ADD COLUMN service character varying(255);
ALTER TABLE atlas_app.payment_order ADD COLUMN client_id character varying(255);
ALTER TABLE atlas_app.payment_order ADD COLUMN description character varying(1024);
ALTER TABLE atlas_app.payment_order ADD COLUMN return_url character varying(255);
ALTER TABLE atlas_app.payment_order ADD COLUMN action character varying(255);
ALTER TABLE atlas_app.payment_order ADD COLUMN request_id character varying(255);
