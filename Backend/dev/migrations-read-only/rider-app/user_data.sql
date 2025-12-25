CREATE TABLE atlas_app.user_data ();

ALTER TABLE atlas_app.user_data ADD COLUMN chakra text NOT NULL;
ALTER TABLE atlas_app.user_data ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.user_data ADD COLUMN user_data_value text NOT NULL;
ALTER TABLE atlas_app.user_data ADD COLUMN user_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.user_data ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.user_data ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.user_data ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.user_data ADD COLUMN event_id character varying(36) ;
ALTER TABLE atlas_app.user_data ADD COLUMN batch_number integer ;
ALTER TABLE atlas_app.user_data ALTER COLUMN event_id SET NOT NULL;
ALTER TABLE atlas_app.user_data ALTER COLUMN batch_number SET NOT NULL;
ALTER TABLE atlas_app.user_data ALTER COLUMN user_data_value TYPE json USING user_data_value::json;


------- SQL updates -------

ALTER TABLE atlas_app.user_data ALTER COLUMN user_data_value TYPE json;