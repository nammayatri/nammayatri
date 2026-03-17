CREATE TABLE atlas_app.frfs_ticket_category_metadata_config ();

ALTER TABLE atlas_app.frfs_ticket_category_metadata_config ADD COLUMN category text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_category_metadata_config ADD COLUMN code text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_category_metadata_config ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_category_metadata_config ADD COLUMN domain_category_value text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_category_metadata_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_category_metadata_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_category_metadata_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_category_metadata_config ADD COLUMN title text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_category_metadata_config ADD COLUMN tnc text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_category_metadata_config ADD COLUMN vehicle_category text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_category_metadata_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_ticket_category_metadata_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_ticket_category_metadata_config ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket_category_metadata_config ADD COLUMN category_order integer ;