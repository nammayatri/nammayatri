CREATE TABLE atlas_app.beckn_config ();

ALTER TABLE atlas_app.beckn_config ADD COLUMN domain text NOT NULL;
ALTER TABLE atlas_app.beckn_config ADD COLUMN gateway_url text NOT NULL;
ALTER TABLE atlas_app.beckn_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.beckn_config ADD COLUMN registry_url text NOT NULL;
ALTER TABLE atlas_app.beckn_config ADD COLUMN subscriber_id text NOT NULL;
ALTER TABLE atlas_app.beckn_config ADD COLUMN subscriber_url text NOT NULL;
ALTER TABLE atlas_app.beckn_config ADD COLUMN unique_key_id text NOT NULL;
ALTER TABLE atlas_app.beckn_config ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.beckn_config ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.beckn_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.beckn_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.beckn_config ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.beckn_config ADD COLUMN settlement_type text ;
ALTER TABLE atlas_app.beckn_config ADD COLUMN payment_params_json text ;



------- SQL updates -------

ALTER TABLE atlas_app.beckn_config ADD COLUMN init_ttl_sec integer ;
ALTER TABLE atlas_app.beckn_config ADD COLUMN confirm_ttl_sec integer ;


------- SQL updates -------

ALTER TABLE atlas_app.beckn_config ADD COLUMN buyer_finder_fee integer ;
ALTER TABLE atlas_app.beckn_config ADD COLUMN bap_ifsc text ;
