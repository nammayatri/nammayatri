CREATE TABLE atlas_driver_offer_bpp.integrated_bpp_config ();

ALTER TABLE atlas_driver_offer_bpp.integrated_bpp_config ADD COLUMN agency_key text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.integrated_bpp_config ADD COLUMN domain text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.integrated_bpp_config ADD COLUMN feed_key text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.integrated_bpp_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.integrated_bpp_config ADD COLUMN is_ticket_valid_on_multiple_routes boolean ;
ALTER TABLE atlas_driver_offer_bpp.integrated_bpp_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.integrated_bpp_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.integrated_bpp_config ADD COLUMN platform_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.integrated_bpp_config ADD COLUMN config_json json NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.integrated_bpp_config ADD COLUMN provider_name text ;
ALTER TABLE atlas_driver_offer_bpp.integrated_bpp_config ADD COLUMN vehicle_category text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.integrated_bpp_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.integrated_bpp_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.integrated_bpp_config ADD PRIMARY KEY ( id);
CREATE INDEX CONCURRENTLY integrated_bpp_config_idx_agency_key ON atlas_driver_offer_bpp.integrated_bpp_config USING btree (agency_key);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.integrated_bpp_config ADD COLUMN city text ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

CREATE INDEX CONCURRENTLY integrated_bpp_config_idx_city_domain_platform_type_vehicle_category ON atlas_driver_offer_bpp.integrated_bpp_config USING btree (city, domain, platform_type, vehicle_category);


------- SQL updates -------

