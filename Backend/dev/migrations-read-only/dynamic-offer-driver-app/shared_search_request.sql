CREATE TABLE atlas_driver_offer_bpp.shared_search_request ();

ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN created_at timestamptz NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN estimated_distance double precision ;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN estimated_duration integer ;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN from_location_ids uuid[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN status character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN to_location_ids uuid[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN toll_charges double precision ;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN toll_names text[] ;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN transaction_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN trip_category character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN updated_at timestamptz NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN valid_till timestamptz NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD COLUMN vehicle_category character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.shared_search_request ADD PRIMARY KEY ( id);
