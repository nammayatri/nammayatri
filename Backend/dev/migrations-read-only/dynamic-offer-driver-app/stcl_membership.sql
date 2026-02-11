CREATE TABLE atlas_driver_offer_bpp.stcl_membership ();

ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN aadhar_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN aadhar_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN account_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN account_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN address_city text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN address_postal_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN address_state text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN address_street_address1 text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN address_street_address2 text ;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN application_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN bank_branch text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN bank_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN date_of_birth date NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN declaration_date date NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN declaration_place text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN declaration_signature text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN email_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN father_mother_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN first_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN fuel_types text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN ifsc_code_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN ifsc_code_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN last_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN member_category text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN mobile_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN mobile_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN nominee_aadhar_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN nominee_aadhar_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN nominee_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN number_of_shares integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN pan_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN pan_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN status text NOT NULL default 'SUBMITTED';
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN terms_accepted boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN short_id text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN payment_status text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN share_start_count integer ;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN share_end_count integer ;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership ADD COLUMN application_count integer ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.stcl_membership DROP COLUMN nominee_aadhar_encrypted;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership DROP COLUMN nominee_aadhar_hash;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership DROP COLUMN aadhar_number_encrypted;
ALTER TABLE atlas_driver_offer_bpp.stcl_membership DROP COLUMN aadhar_number_hash;