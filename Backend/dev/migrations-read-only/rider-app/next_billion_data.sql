CREATE TABLE atlas_app.next_billion_data ();

ALTER TABLE atlas_app.next_billion_data ADD COLUMN routes text[] NOT NULL;
ALTER TABLE atlas_app.next_billion_data ADD COLUMN search_request_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.next_billion_data ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.next_billion_data ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.next_billion_data ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.next_billion_data ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.next_billion_data ADD PRIMARY KEY ( search_request_id);