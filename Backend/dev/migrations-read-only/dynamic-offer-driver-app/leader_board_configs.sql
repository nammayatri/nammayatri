CREATE TABLE atlas_driver_offer_bpp.leader_board_configs ();

ALTER TABLE atlas_driver_offer_bpp.leader_board_configs ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.leader_board_configs ADD COLUMN is_enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.leader_board_configs ADD COLUMN leader_board_expiry integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.leader_board_configs ADD COLUMN leader_board_length_limit integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.leader_board_configs ADD COLUMN leader_board_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.leader_board_configs ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.leader_board_configs ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.leader_board_configs ADD COLUMN number_of_sets integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.leader_board_configs ADD COLUMN z_score_base integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.leader_board_configs ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.leader_board_configs ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.leader_board_configs ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.leader_board_configs ADD COLUMN use_operating_city_based_leader_board boolean ;
