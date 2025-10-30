CREATE TABLE atlas_app.purchased_pass_payment ();

ALTER TABLE atlas_app.purchased_pass_payment ADD COLUMN end_date date NOT NULL;
ALTER TABLE atlas_app.purchased_pass_payment ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass_payment ADD COLUMN order_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass_payment ADD COLUMN purchased_pass_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass_payment ADD COLUMN start_date date NOT NULL;
ALTER TABLE atlas_app.purchased_pass_payment ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.purchased_pass_payment ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.purchased_pass_payment ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.purchased_pass_payment ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.purchased_pass_payment ADD PRIMARY KEY ( id);
