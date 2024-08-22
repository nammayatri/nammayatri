CREATE TABLE atlas_app.white_list_org ();

ALTER TABLE atlas_app.white_list_org ADD COLUMN created_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.white_list_org ADD COLUMN domain character varying(255) NOT NULL;
ALTER TABLE atlas_app.white_list_org ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.white_list_org ADD COLUMN merchant_id character varying(36) NOT NULL default '';
ALTER TABLE atlas_app.white_list_org ADD COLUMN subscriber_id character varying(255) NOT NULL;
ALTER TABLE atlas_app.white_list_org ADD COLUMN updated_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.white_list_org ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.white_list_org ADD COLUMN merchant_operating_city_id character varying(36);

------- SQL updates -------

ALTER TABLE atlas_app.white_list_org ALTER COLUMN merchant_operating_city_id SET DEFAULT '';
ALTER TABLE atlas_app.white_list_org ALTER COLUMN merchant_operating_city_id SET NOT NULL;
