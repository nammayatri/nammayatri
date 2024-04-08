CREATE TABLE atlas_app.special_zone_quote ();

ALTER TABLE atlas_app.special_zone_quote ADD COLUMN created_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.special_zone_quote ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.special_zone_quote ADD COLUMN quote_id text NOT NULL;
ALTER TABLE atlas_app.special_zone_quote ADD COLUMN updated_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.special_zone_quote ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.special_zone_quote ALTER COLUMN quote_id TYPE character varying(100);