CREATE TABLE atlas_app.callback_request ();

ALTER TABLE atlas_app.callback_request ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.callback_request ADD COLUMN customer_mobile_country_code text NOT NULL;
ALTER TABLE atlas_app.callback_request ADD COLUMN customer_name text ;
ALTER TABLE atlas_app.callback_request ADD COLUMN customer_phone text NOT NULL;
ALTER TABLE atlas_app.callback_request ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.callback_request ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.callback_request ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.callback_request ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.callback_request ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.callback_request ADD COLUMN customer_phone_hash bytea NOT NULL;
ALTER TABLE atlas_app.callback_request ADD COLUMN customer_phone_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_app.callback_request DROP COLUMN customer_phone;