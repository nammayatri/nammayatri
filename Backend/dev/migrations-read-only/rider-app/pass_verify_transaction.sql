CREATE TABLE atlas_app.pass_verify_transaction ();

ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN fleet_id text NOT NULL;
ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN purchase_pass_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN verified_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass_verify_transaction ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN source_stop_code text ;
ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN destination_stop_code text ;


------- SQL updates -------

ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN purchase_pass_payment_id character varying(36) ;
ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN opening_amount double precision ;
ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN exit_gate_id text ;
ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN entry_gate_id text ;
ALTER TABLE atlas_app.pass_verify_transaction ADD COLUMN closing_amount double precision ;