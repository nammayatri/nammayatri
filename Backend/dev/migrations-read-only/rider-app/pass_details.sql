CREATE TABLE atlas_app.pass_details ();

ALTER TABLE atlas_app.pass_details ADD COLUMN address text ;
ALTER TABLE atlas_app.pass_details ADD COLUMN age integer ;
ALTER TABLE atlas_app.pass_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass_details ADD COLUMN graduation_date timestamp with time zone ;
ALTER TABLE atlas_app.pass_details ADD COLUMN guardian_name text ;
ALTER TABLE atlas_app.pass_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN id_card_picture text ;
ALTER TABLE atlas_app.pass_details ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN number_of_stages integer ;
ALTER TABLE atlas_app.pass_details ADD COLUMN pass_enum text NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN pass_organization_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN register_no text ;
ALTER TABLE atlas_app.pass_details ADD COLUMN remark text ;
ALTER TABLE atlas_app.pass_details ADD COLUMN route_pairs jsonb ;
ALTER TABLE atlas_app.pass_details ADD COLUMN student_class text ;
ALTER TABLE atlas_app.pass_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass_details ADD COLUMN verification_date timestamp with time zone ;
ALTER TABLE atlas_app.pass_details ADD COLUMN verification_status text NOT NULL;
ALTER TABLE atlas_app.pass_details ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.pass_details ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.pass_details ADD PRIMARY KEY ( id, person_id);
CREATE INDEX pass_details_idx_id ON atlas_app.pass_details USING btree (id);
CREATE INDEX pass_details_idx_pass_organization_id ON atlas_app.pass_details USING btree (pass_organization_id);


------- SQL updates -------

ALTER TABLE atlas_app.pass_details ADD COLUMN applicable_route_ids text[] ;