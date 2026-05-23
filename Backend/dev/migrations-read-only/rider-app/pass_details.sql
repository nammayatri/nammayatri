CREATE TABLE atlas_app.pass_details ();

ALTER TABLE atlas_app.pass_details ADD COLUMN aadhar_no_encrypted character varying(255) ;
ALTER TABLE atlas_app.pass_details ADD COLUMN aadhar_no_hash bytea ;
ALTER TABLE atlas_app.pass_details ADD COLUMN academic_year_end date ;
ALTER TABLE atlas_app.pass_details ADD COLUMN academic_year_start date ;
ALTER TABLE atlas_app.pass_details ADD COLUMN address jsonb ;
ALTER TABLE atlas_app.pass_details ADD COLUMN age integer ;
ALTER TABLE atlas_app.pass_details ADD COLUMN applicable_route_ids text[] ;
ALTER TABLE atlas_app.pass_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass_details ADD COLUMN department text ;
ALTER TABLE atlas_app.pass_details ADD COLUMN gender text NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN guardian_mobile_number_encrypted character varying(255) ;
ALTER TABLE atlas_app.pass_details ADD COLUMN guardian_mobile_number_hash bytea ;
ALTER TABLE atlas_app.pass_details ADD COLUMN guardian_name text ;
ALTER TABLE atlas_app.pass_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN id_card_picture text ;
ALTER TABLE atlas_app.pass_details ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN number_of_stages integer ;
ALTER TABLE atlas_app.pass_details ADD COLUMN pass_enum text NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN pass_organization_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN pincode text ;
ALTER TABLE atlas_app.pass_details ADD COLUMN reference_number integer ;
ALTER TABLE atlas_app.pass_details ADD COLUMN register_no text ;
ALTER TABLE atlas_app.pass_details ADD COLUMN remark text ;
ALTER TABLE atlas_app.pass_details ADD COLUMN route_pairs jsonb ;
ALTER TABLE atlas_app.pass_details ADD COLUMN self_image text NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass_details ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN verification_status text NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN year text ;
ALTER TABLE atlas_app.pass_details ADD PRIMARY KEY ( id);
CREATE INDEX CONCURRENTLY pass_details_idx_pass_organization_id ON atlas_app.pass_details USING btree (pass_organization_id);
CREATE INDEX CONCURRENTLY pass_details_idx_person_id ON atlas_app.pass_details USING btree (person_id);
CREATE INDEX CONCURRENTLY pass_details_idx_pass_enum_person_id ON atlas_app.pass_details USING btree (pass_enum, person_id);
CREATE INDEX CONCURRENTLY pass_details_idx_reference_number ON atlas_app.pass_details USING btree (reference_number);


------- SQL updates -------

ALTER TABLE atlas_app.pass_details ALTER COLUMN self_image TYPE character varying(36);
ALTER TABLE atlas_app.pass_details ALTER COLUMN id_card_picture TYPE character varying(36);