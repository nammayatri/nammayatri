CREATE TABLE atlas_app.client_person_info ();

ALTER TABLE atlas_app.client_person_info ADD COLUMN client_id character varying(36) ;
ALTER TABLE atlas_app.client_person_info ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.client_person_info ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.client_person_info ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.client_person_info ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.client_person_info ADD COLUMN ride_count integer NOT NULL;
ALTER TABLE atlas_app.client_person_info ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.client_person_info ADD COLUMN vehicle_category text ;
ALTER TABLE atlas_app.client_person_info ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.client_person_info ADD COLUMN merchant_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_app.client_person_info ALTER COLUMN merchant_id SET NOT NULL;