CREATE TABLE atlas_app.frfs_trip ();

ALTER TABLE atlas_app.frfs_trip ADD COLUMN bpp_fulfillment_id text NOT NULL;
ALTER TABLE atlas_app.frfs_trip ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_trip ADD COLUMN quote_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_trip ADD COLUMN sequence integer NOT NULL;
ALTER TABLE atlas_app.frfs_trip ADD COLUMN station_code text NOT NULL;
ALTER TABLE atlas_app.frfs_trip ADD COLUMN station_type text NOT NULL;
ALTER TABLE atlas_app.frfs_trip ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.frfs_trip ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.frfs_trip ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_trip ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_trip ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.frfs_trip ADD COLUMN stop_sequence integer NOT NULL;
ALTER TABLE atlas_app.frfs_trip DROP COLUMN sequence;