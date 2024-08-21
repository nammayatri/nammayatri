CREATE TABLE atlas_app.search_request_delivery_details ();

ALTER TABLE atlas_app.search_request_delivery_details ADD COLUMN initiated_as text NOT NULL;
ALTER TABLE atlas_app.search_request_delivery_details ADD COLUMN receiver_name text NOT NULL;
ALTER TABLE atlas_app.search_request_delivery_details ADD COLUMN receiver_phone_number_encrypted text NOT NULL;
ALTER TABLE atlas_app.search_request_delivery_details ADD COLUMN receiver_phone_number_hash bytea NOT NULL;
ALTER TABLE atlas_app.search_request_delivery_details ADD COLUMN search_request_id text NOT NULL;
ALTER TABLE atlas_app.search_request_delivery_details ADD COLUMN sender_name text NOT NULL;
ALTER TABLE atlas_app.search_request_delivery_details ADD COLUMN sender_phone_number_encrypted text NOT NULL;
ALTER TABLE atlas_app.search_request_delivery_details ADD COLUMN sender_phone_number_hash bytea NOT NULL;
ALTER TABLE atlas_app.search_request_delivery_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.search_request_delivery_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.search_request_delivery_details ADD PRIMARY KEY ( search_request_id);