CREATE TABLE atlas_app.journey_leg_mapping ();

ALTER TABLE atlas_app.journey_leg_mapping ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey_leg_mapping ADD COLUMN is_deleted boolean NOT NULL;
ALTER TABLE atlas_app.journey_leg_mapping ADD COLUMN journey_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey_leg_mapping ADD COLUMN journey_leg_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey_leg_mapping ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey_leg_mapping ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey_leg_mapping ADD COLUMN sequence_number integer NOT NULL;
ALTER TABLE atlas_app.journey_leg_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journey_leg_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journey_leg_mapping ADD PRIMARY KEY ( id);
