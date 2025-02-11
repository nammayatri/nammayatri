CREATE TABLE atlas_app.rollout ();

ALTER TABLE atlas_app.rollout ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.rollout ADD COLUMN input_data_type text NOT NULL;
ALTER TABLE atlas_app.rollout ADD COLUMN percentage integer NOT NULL;
ALTER TABLE atlas_app.rollout ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.rollout ADD COLUMN version_tag integer NOT NULL;
ALTER TABLE atlas_app.rollout ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.rollout ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.rollout ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.rollout ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.rollout ADD PRIMARY KEY ( id);
