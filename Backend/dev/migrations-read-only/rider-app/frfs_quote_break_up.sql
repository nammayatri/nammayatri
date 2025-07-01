CREATE TABLE atlas_app.frfs_quote_break_up ();

ALTER TABLE atlas_app.frfs_quote_break_up ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.frfs_quote_break_up ADD COLUMN currency text ;
ALTER TABLE atlas_app.frfs_quote_break_up ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.frfs_quote_break_up ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote_break_up ADD COLUMN quote_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_quote_break_up ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.frfs_quote_break_up ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.frfs_quote_break_up ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_quote_break_up ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_quote_break_up ADD PRIMARY KEY ( id);
