CREATE TABLE atlas_driver_offer_bpp.driver_fy_earnings ();

ALTER TABLE atlas_driver_offer_bpp.driver_fy_earnings ADD COLUMN financial_year integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fy_earnings ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fy_earnings ADD COLUMN net_earnings_total numeric(30,2) NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_fy_earnings ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fy_earnings ADD COLUMN quarter integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_fy_earnings ADD COLUMN tds_amount_total numeric(30,2) NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_fy_earnings ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_fy_earnings ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_fy_earnings ADD PRIMARY KEY ( id);
ALTER TABLE atlas_driver_offer_bpp.driver_fy_earnings ADD CONSTRAINT driver_fy_earnings_unique_idx_financial_year_person_id_quarter UNIQUE (financial_year, person_id, quarter);