CREATE TABLE atlas_app.frfs_search ();

ALTER TABLE atlas_app.frfs_search ADD COLUMN from character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_search ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_search ADD COLUMN quantity integer NOT NULL;
ALTER TABLE atlas_app.frfs_search ADD COLUMN to character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_search ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.frfs_search ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.frfs_search ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_search ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_search ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.frfs_search ADD COLUMN to_station_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_search ADD COLUMN from_station_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_search DROP COLUMN to;
ALTER TABLE atlas_app.frfs_search DROP COLUMN from;