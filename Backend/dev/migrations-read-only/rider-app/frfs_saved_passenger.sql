CREATE TABLE atlas_app.frfs_saved_passenger ();

ALTER TABLE atlas_app.frfs_saved_passenger ADD COLUMN age integer NOT NULL;
ALTER TABLE atlas_app.frfs_saved_passenger ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_saved_passenger ADD COLUMN gender text NOT NULL;
ALTER TABLE atlas_app.frfs_saved_passenger ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_saved_passenger ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_saved_passenger ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_saved_passenger ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.frfs_saved_passenger ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_saved_passenger ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_saved_passenger ADD PRIMARY KEY ( id);
CREATE INDEX CONCURRENTLY frfs_saved_passenger_idx_rider_id ON atlas_app.frfs_saved_passenger USING btree (rider_id);