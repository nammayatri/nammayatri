CREATE TABLE atlas_driver_offer_bpp.add_on_config ();

ALTER TABLE atlas_driver_offer_bpp.add_on_config ADD COLUMN add_on_type character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.add_on_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.add_on_config ADD COLUMN descriptor_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.add_on_config ADD COLUMN descriptor_short_desc text ;
ALTER TABLE atlas_driver_offer_bpp.add_on_config ADD COLUMN enabled boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.add_on_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.add_on_config ADD COLUMN max_quantity integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.add_on_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.add_on_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.add_on_config ADD COLUMN metadata jsonb ;
ALTER TABLE atlas_driver_offer_bpp.add_on_config ADD COLUMN price_per_quantity double precision ;
ALTER TABLE atlas_driver_offer_bpp.add_on_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.add_on_config ADD COLUMN vehicle_service_tier text[] NOT NULL default '{}';
ALTER TABLE atlas_driver_offer_bpp.add_on_config ADD PRIMARY KEY ( id);
