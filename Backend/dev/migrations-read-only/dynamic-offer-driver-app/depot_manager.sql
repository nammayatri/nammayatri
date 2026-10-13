CREATE TABLE atlas_driver_offer_bpp.depot_manager ();

ALTER TABLE atlas_driver_offer_bpp.depot_manager ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.depot_manager ADD COLUMN depot_code character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.depot_manager ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.depot_manager ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.depot_manager ADD COLUMN is_admin boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.depot_manager ADD COLUMN is_block_allowed boolean ;
ALTER TABLE atlas_driver_offer_bpp.depot_manager ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.depot_manager ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.depot_manager ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.depot_manager ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.depot_manager ADD PRIMARY KEY ( id);



------- SQL updates -------

CREATE INDEX CONCURRENTLY depot_manager_idx_depot_code ON atlas_driver_offer_bpp.depot_manager USING btree (depot_code);
CREATE INDEX CONCURRENTLY depot_manager_idx_person_id ON atlas_driver_offer_bpp.depot_manager USING btree (person_id);