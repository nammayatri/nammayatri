CREATE TABLE atlas_app.purchased_pass ();

ALTER TABLE atlas_app.purchased_pass ADD COLUMN expiry_date timestamp with time zone ;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN pass_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN valid_trips_left integer ;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.purchased_pass ADD PRIMARY KEY ( id);
