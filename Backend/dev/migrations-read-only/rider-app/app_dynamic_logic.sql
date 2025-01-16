CREATE TABLE atlas_app.app_dynamic_logic ();

ALTER TABLE atlas_app.app_dynamic_logic ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.app_dynamic_logic ADD COLUMN domain text NOT NULL;
ALTER TABLE atlas_app.app_dynamic_logic ADD COLUMN logic text NOT NULL;
ALTER TABLE atlas_app.app_dynamic_logic ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.app_dynamic_logic ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.app_dynamic_logic ADD COLUMN "order" integer NOT NULL;
ALTER TABLE atlas_app.app_dynamic_logic ADD COLUMN time_bounds text  default 'Unbounded';
ALTER TABLE atlas_app.app_dynamic_logic ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.app_dynamic_logic ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.app_dynamic_logic ADD PRIMARY KEY ( domain, merchant_operating_city_id, name);