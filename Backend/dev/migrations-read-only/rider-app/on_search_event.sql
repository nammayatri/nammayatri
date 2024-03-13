CREATE TABLE atlas_app.on_search_event ();

ALTER TABLE atlas_app.on_search_event ADD COLUMN bpp_id text NOT NULL;
ALTER TABLE atlas_app.on_search_event ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.on_search_event ADD COLUMN error_code text ;
ALTER TABLE atlas_app.on_search_event ADD COLUMN error_message text ;
ALTER TABLE atlas_app.on_search_event ADD COLUMN error_type text ;
ALTER TABLE atlas_app.on_search_event ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.on_search_event ADD COLUMN message_id text NOT NULL;
ALTER TABLE atlas_app.on_search_event ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.on_search_event ADD PRIMARY KEY ( id);