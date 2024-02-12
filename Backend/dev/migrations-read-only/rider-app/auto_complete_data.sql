CREATE TABLE atlas_app.auto_complete_data (); -- IMP: enable kv for this table

ALTER TABLE atlas_app.auto_complete_data ADD COLUMN autocomplete_inputs text NOT NULL;
ALTER TABLE atlas_app.auto_complete_data ADD COLUMN customer_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.auto_complete_data ADD COLUMN id text NOT NULL;
ALTER TABLE atlas_app.auto_complete_data ADD COLUMN is_location_selected_on_map boolean ;
ALTER TABLE atlas_app.auto_complete_data ADD COLUMN search_request_id character varying(36) ;
ALTER TABLE atlas_app.auto_complete_data ADD COLUMN search_type text ;
ALTER TABLE atlas_app.auto_complete_data ADD COLUMN session_token text NOT NULL;
ALTER TABLE atlas_app.auto_complete_data ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.auto_complete_data ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.auto_complete_data ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.auto_complete_data ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.auto_complete_data ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.auto_complete_data ALTER COLUMN search_type SET NOT NULL;