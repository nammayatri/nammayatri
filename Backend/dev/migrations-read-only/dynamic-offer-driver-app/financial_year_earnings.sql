CREATE TABLE atlas_driver_offer_bpp.financial_year_earnings ();

ALTER TABLE atlas_driver_offer_bpp.financial_year_earnings ADD COLUMN financial_year_collection_amount numeric(30,2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.financial_year_earnings ADD COLUMN financial_year_start integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.financial_year_earnings ADD COLUMN financial_year_tds_base_amount numeric(30,2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.financial_year_earnings ADD COLUMN financial_year_tds_deduction numeric(30,2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.financial_year_earnings ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.financial_year_earnings ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.financial_year_earnings ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.financial_year_earnings ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.financial_year_earnings ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.financial_year_earnings ADD PRIMARY KEY ( id);
