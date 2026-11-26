CREATE TABLE atlas_app.shared_cab_slot_capacity ();

ALTER TABLE atlas_app.shared_cab_slot_capacity ADD COLUMN capacity integer NOT NULL;
ALTER TABLE atlas_app.shared_cab_slot_capacity ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.shared_cab_slot_capacity ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.shared_cab_slot_capacity ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.shared_cab_slot_capacity ADD COLUMN route_id text NOT NULL;
ALTER TABLE atlas_app.shared_cab_slot_capacity ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.shared_cab_slot_capacity ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.shared_cab_slot_capacity ADD PRIMARY KEY ( id);
