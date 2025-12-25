CREATE TABLE atlas_app.deleted_person ();

ALTER TABLE atlas_app.deleted_person ADD COLUMN client_os_type text ;
ALTER TABLE atlas_app.deleted_person ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.deleted_person ADD COLUMN device_id text ;
ALTER TABLE atlas_app.deleted_person ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.deleted_person ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.deleted_person ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.deleted_person ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.deleted_person ADD PRIMARY KEY ( person_id);

------- SQL updates -------

ALTER TABLE atlas_app.deleted_person ADD COLUMN reason_to_delete text ;
