CREATE TABLE atlas_driver_offer_bpp.finance_account ();

ALTER TABLE atlas_driver_offer_bpp.finance_account ADD COLUMN account_category text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_account ADD COLUMN account_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_account ADD COLUMN balance double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_account ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.finance_account ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_account ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_account ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_account ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_account ADD COLUMN owner_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_account ADD COLUMN owner_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_account ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_account ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.finance_account ADD PRIMARY KEY ( id);
