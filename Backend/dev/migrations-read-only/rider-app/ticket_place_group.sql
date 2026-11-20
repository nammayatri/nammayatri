CREATE TABLE atlas_app.ticket_place_group ();

ALTER TABLE atlas_app.ticket_place_group ADD COLUMN description text ;
ALTER TABLE atlas_app.ticket_place_group ADD COLUMN geom_geo_json text ;
ALTER TABLE atlas_app.ticket_place_group ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_place_group ADD COLUMN is_active boolean NOT NULL default true;
ALTER TABLE atlas_app.ticket_place_group ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_place_group ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.ticket_place_group ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ticket_place_group ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_place_group ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_place_group ADD PRIMARY KEY ( id);
