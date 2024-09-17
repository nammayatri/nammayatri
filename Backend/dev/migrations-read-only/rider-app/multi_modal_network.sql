CREATE TABLE atlas_app.multi_modal_network ();

ALTER TABLE atlas_app.multi_modal_network ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.multi_modal_network ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.multi_modal_network ADD COLUMN network_class text NOT NULL;
ALTER TABLE atlas_app.multi_modal_network ADD COLUMN network_code text NOT NULL;
ALTER TABLE atlas_app.multi_modal_network ADD COLUMN network_type text NOT NULL;
ALTER TABLE atlas_app.multi_modal_network ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.multi_modal_network ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.multi_modal_network ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multi_modal_network ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multi_modal_network ADD PRIMARY KEY ( id);