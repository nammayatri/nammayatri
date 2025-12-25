CREATE TABLE atlas_app.bpp_details ();

ALTER TABLE atlas_app.bpp_details ADD COLUMN description text ;
ALTER TABLE atlas_app.bpp_details ADD COLUMN domain text NOT NULL;
ALTER TABLE atlas_app.bpp_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.bpp_details ADD COLUMN logo_url text ;
ALTER TABLE atlas_app.bpp_details ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.bpp_details ADD COLUMN subscriber_id text NOT NULL;
ALTER TABLE atlas_app.bpp_details ADD COLUMN support_number text ;
ALTER TABLE atlas_app.bpp_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.bpp_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.bpp_details ADD PRIMARY KEY ( id);