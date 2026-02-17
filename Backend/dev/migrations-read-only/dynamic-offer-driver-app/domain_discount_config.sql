CREATE TABLE atlas_driver_offer_bpp.domain_discount_config ();

ALTER TABLE atlas_driver_offer_bpp.domain_discount_config ADD COLUMN billing_category text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.domain_discount_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.domain_discount_config ADD COLUMN discount_percentage double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.domain_discount_config ADD COLUMN domain text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.domain_discount_config ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.domain_discount_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.domain_discount_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.domain_discount_config ADD COLUMN vehicle_service_tier text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.domain_discount_config ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.domain_discount_config ADD PRIMARY KEY ( billing_category, domain, merchant_operating_city_id, vehicle_service_tier);
