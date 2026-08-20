CREATE TABLE atlas_app.meta_config ();

ALTER TABLE atlas_app.meta_config ADD COLUMN access_token text ;
ALTER TABLE atlas_app.meta_config ADD COLUMN api_version text ;
ALTER TABLE atlas_app.meta_config ADD COLUMN app_secret text NOT NULL;
ALTER TABLE atlas_app.meta_config ADD COLUMN base_url text ;
ALTER TABLE atlas_app.meta_config ADD COLUMN bot_config jsonb NOT NULL;
ALTER TABLE atlas_app.meta_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.meta_config ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_app.meta_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.meta_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.meta_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.meta_config ADD COLUMN phone_number_id text NOT NULL;
ALTER TABLE atlas_app.meta_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.meta_config ADD COLUMN verify_token text NOT NULL;
ALTER TABLE atlas_app.meta_config ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.meta_config ALTER COLUMN base_url SET DEFAULT '';
ALTER TABLE atlas_app.meta_config ALTER COLUMN base_url SET NOT NULL;
ALTER TABLE atlas_app.meta_config ALTER COLUMN api_version SET DEFAULT '';
ALTER TABLE atlas_app.meta_config ALTER COLUMN api_version SET NOT NULL;
ALTER TABLE atlas_app.meta_config ALTER COLUMN access_token SET DEFAULT '';
ALTER TABLE atlas_app.meta_config ALTER COLUMN access_token SET NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.meta_config ALTER COLUMN base_url DROP DEFAULT;
ALTER TABLE atlas_app.meta_config ALTER COLUMN api_version DROP DEFAULT;
ALTER TABLE atlas_app.meta_config ALTER COLUMN access_token DROP DEFAULT;