CREATE TABLE atlas_app.frfs_gtfs_stage_fare ();

ALTER TABLE atlas_app.frfs_gtfs_stage_fare ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.frfs_gtfs_stage_fare ADD COLUMN cess_charge double precision ;
ALTER TABLE atlas_app.frfs_gtfs_stage_fare ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_app.frfs_gtfs_stage_fare ADD COLUMN discount_ids text[] NOT NULL;
ALTER TABLE atlas_app.frfs_gtfs_stage_fare ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_gtfs_stage_fare ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_gtfs_stage_fare ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_gtfs_stage_fare ADD COLUMN stage integer NOT NULL;
ALTER TABLE atlas_app.frfs_gtfs_stage_fare ADD COLUMN vehicle_service_tier_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_gtfs_stage_fare ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.frfs_gtfs_stage_fare ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_gtfs_stage_fare ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_gtfs_stage_fare ADD PRIMARY KEY ( id);
