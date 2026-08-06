CREATE TABLE atlas_dashboard.registration_token ();

ALTER TABLE atlas_dashboard.registration_token ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_dashboard.registration_token ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_dashboard.registration_token ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_dashboard.registration_token ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_dashboard.registration_token ADD COLUMN operating_city text NOT NULL;
ALTER TABLE atlas_dashboard.registration_token ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_dashboard.registration_token ADD COLUMN token text NOT NULL;
ALTER TABLE atlas_dashboard.registration_token ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_dashboard.registration_token ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_dashboard.registration_token ADD PRIMARY KEY ( id);



------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

