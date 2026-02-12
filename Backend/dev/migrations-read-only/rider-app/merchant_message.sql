CREATE TABLE atlas_app.merchant_message ();

ALTER TABLE atlas_app.merchant_message ADD COLUMN contains_url_button boolean NOT NULL default False;
ALTER TABLE atlas_app.merchant_message ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_message ADD COLUMN json_data json ;
ALTER TABLE atlas_app.merchant_message ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_message ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_message ADD COLUMN message text NOT NULL;
ALTER TABLE atlas_app.merchant_message ADD COLUMN message_key character varying(255) NOT NULL;
ALTER TABLE atlas_app.merchant_message ADD COLUMN template_id character varying(255) ;
ALTER TABLE atlas_app.merchant_message ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_message ADD PRIMARY KEY ( merchant_operating_city_id, message_key);


------- SQL updates -------

ALTER TABLE atlas_app.merchant_message ADD COLUMN sender_header text ;


------- SQL updates -------

ALTER TABLE atlas_app.merchant_message ADD COLUMN message_type character varying(255) ;