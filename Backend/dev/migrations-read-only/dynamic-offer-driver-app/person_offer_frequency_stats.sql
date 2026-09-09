CREATE TABLE atlas_driver_offer_bpp.person_offer_frequency_stats ();

ALTER TABLE atlas_driver_offer_bpp.person_offer_frequency_stats ADD COLUMN applied_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_offer_frequency_stats ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.person_offer_frequency_stats ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_offer_frequency_stats ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_offer_frequency_stats ADD COLUMN entity_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_offer_frequency_stats ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_offer_frequency_stats ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_offer_frequency_stats ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_offer_frequency_stats ADD COLUMN offer_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_offer_frequency_stats ADD COLUMN period_start timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.person_offer_frequency_stats ADD COLUMN total_cashback_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_offer_frequency_stats ADD COLUMN total_discount_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_offer_frequency_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.person_offer_frequency_stats ADD PRIMARY KEY ( id);
