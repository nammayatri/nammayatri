CREATE TABLE atlas_app.partner_org_station ();

ALTER TABLE atlas_app.partner_org_station ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.partner_org_station ADD COLUMN partner_org_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.partner_org_station ADD COLUMN partner_org_station_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.partner_org_station ADD COLUMN station_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.partner_org_station ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.partner_org_station ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.partner_org_station ADD PRIMARY KEY ( partner_org_id, partner_org_station_id);


------- SQL updates -------

ALTER TABLE atlas_app.partner_org_station ALTER COLUMN station_id TYPE text;