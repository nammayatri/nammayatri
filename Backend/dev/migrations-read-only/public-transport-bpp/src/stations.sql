CREATE TABLE atlas_public_transport_bpp.stations ();

ALTER TABLE atlas_public_transport_bpp.stations ADD COLUMN code text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.stations ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.stations ADD COLUMN lat double precision ;
ALTER TABLE atlas_public_transport_bpp.stations ADD COLUMN lon double precision ;
ALTER TABLE atlas_public_transport_bpp.stations ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.stations ADD COLUMN merchnat_id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.stations ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.stations ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.stations ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_public_transport_bpp.stations ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_public_transport_bpp.stations ADD PRIMARY KEY ( id);
