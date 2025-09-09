CREATE TABLE atlas_app.frfs_quote_category ();

ALTER TABLE atlas_app.frfs_quote_category ADD COLUMN bpp_item_id text NOT NULL;
ALTER TABLE atlas_app.frfs_quote_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote_category ADD COLUMN max_ticket_allowed integer ;
ALTER TABLE atlas_app.frfs_quote_category ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote_category ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote_category ADD COLUMN offered_price text NOT NULL;
ALTER TABLE atlas_app.frfs_quote_category ADD COLUMN currency text ;
ALTER TABLE atlas_app.frfs_quote_category ADD COLUMN price double precision NOT NULL;
ALTER TABLE atlas_app.frfs_quote_category ADD COLUMN quote_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote_category ADD COLUMN ticket_category_metadata_config_id text NOT NULL;
ALTER TABLE atlas_app.frfs_quote_category ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_quote_category ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_quote_category ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.frfs_quote_category ALTER COLUMN offered_price TYPE double precision;