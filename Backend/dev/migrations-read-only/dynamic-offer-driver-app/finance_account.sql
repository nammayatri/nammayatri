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



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.finance_account ADD COLUMN counterparty_type text ;
ALTER TABLE atlas_driver_offer_bpp.finance_account ADD COLUMN counterparty_id text ;

--- Now DSL don't allow dropping tables instead we will drop not null constraint if any .Please be careful while running ---
ALTER TABLE atlas_driver_offer_bpp.finance_account ALTER COLUMN owner_type DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_account ALTER COLUMN owner_id DROP NOT NULL;
--- Drop section ends. Please check before running ---



------- SQL updates -------

