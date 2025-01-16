CREATE TABLE atlas_app.quote_breakup ();

ALTER TABLE atlas_app.quote_breakup ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.quote_breakup ADD COLUMN price_currency character varying(255) ;
ALTER TABLE atlas_app.quote_breakup ADD COLUMN price_value double precision NOT NULL;
ALTER TABLE atlas_app.quote_breakup ADD COLUMN quote_id text NOT NULL;
ALTER TABLE atlas_app.quote_breakup ADD COLUMN title text NOT NULL;
ALTER TABLE atlas_app.quote_breakup ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.quote_breakup ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.quote_breakup ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.quote_breakup ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.quote_breakup ADD PRIMARY KEY ( id);