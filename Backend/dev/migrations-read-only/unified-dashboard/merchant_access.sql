CREATE TABLE atlas_dashboard.merchant_access ();

ALTER TABLE atlas_dashboard.merchant_access ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_dashboard.merchant_access ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_dashboard.merchant_access ADD COLUMN is2fa_enabled boolean NOT NULL;
ALTER TABLE atlas_dashboard.merchant_access ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_dashboard.merchant_access ADD COLUMN merchant_short_id character varying(36) NOT NULL;
ALTER TABLE atlas_dashboard.merchant_access ADD COLUMN operating_city text NOT NULL;
ALTER TABLE atlas_dashboard.merchant_access ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_dashboard.merchant_access ADD COLUMN secret_key text ;
ALTER TABLE atlas_dashboard.merchant_access ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_dashboard.merchant_access ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_dashboard.merchant_access ADD PRIMARY KEY ( id);



------- SQL updates -------




------- SQL updates -------

