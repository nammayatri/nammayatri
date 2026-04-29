CREATE TABLE atlas_bpp_dashboard.merchant_operating_city ();

ALTER TABLE atlas_bpp_dashboard.merchant_operating_city ADD COLUMN city text NOT NULL;
ALTER TABLE atlas_bpp_dashboard.merchant_operating_city ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_bpp_dashboard.merchant_operating_city ADD COLUMN std_code text ;