-- PayoutMethod table
CREATE TABLE atlas_driver_offer_bpp.payout_method ();

ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN method_type character varying(255) NOT NULL DEFAULT 'UPI_VPA';
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN vpa text ;
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN bank_account_number text ;
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN ifsc_code text ;
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN bank_name text ;
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN beneficiary_name text ;
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN is_primary boolean NOT NULL DEFAULT false;
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN verification_status character varying(255) NOT NULL DEFAULT 'PENDING';
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN is_blocked boolean NOT NULL DEFAULT false;
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN blocked_reason text ;
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD COLUMN updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_method ADD PRIMARY KEY (id);

CREATE INDEX idx_payout_method_driver_id ON atlas_driver_offer_bpp.payout_method USING btree (driver_id);

-- PayoutFeeConfig table
CREATE TABLE atlas_driver_offer_bpp.payout_fee_config ();

ALTER TABLE atlas_driver_offer_bpp.payout_fee_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_fee_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_fee_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_fee_config ADD COLUMN method_type character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_fee_config ADD COLUMN payout_type character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_fee_config ADD COLUMN fee_percentage double precision NOT NULL DEFAULT 0.0;
ALTER TABLE atlas_driver_offer_bpp.payout_fee_config ADD COLUMN fixed_fee double precision NOT NULL DEFAULT 0.0;
ALTER TABLE atlas_driver_offer_bpp.payout_fee_config ADD COLUMN min_fee double precision NOT NULL DEFAULT 0.0;
ALTER TABLE atlas_driver_offer_bpp.payout_fee_config ADD COLUMN max_fee double precision NOT NULL DEFAULT 0.0;
ALTER TABLE atlas_driver_offer_bpp.payout_fee_config ADD COLUMN created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_fee_config ADD COLUMN updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_fee_config ADD PRIMARY KEY (id);

CREATE INDEX idx_payout_fee_config_moc_id ON atlas_driver_offer_bpp.payout_fee_config USING btree (merchant_operating_city_id);

-- PayoutLimit table
CREATE TABLE atlas_driver_offer_bpp.payout_limit ();

ALTER TABLE atlas_driver_offer_bpp.payout_limit ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_limit ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_limit ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_limit ADD COLUMN daily_limit double precision NOT NULL DEFAULT 0.0;
ALTER TABLE atlas_driver_offer_bpp.payout_limit ADD COLUMN weekly_limit double precision NOT NULL DEFAULT 0.0;
ALTER TABLE atlas_driver_offer_bpp.payout_limit ADD COLUMN monthly_limit double precision NOT NULL DEFAULT 0.0;
ALTER TABLE atlas_driver_offer_bpp.payout_limit ADD COLUMN per_transaction_min double precision NOT NULL DEFAULT 0.0;
ALTER TABLE atlas_driver_offer_bpp.payout_limit ADD COLUMN per_transaction_max double precision NOT NULL DEFAULT 0.0;
ALTER TABLE atlas_driver_offer_bpp.payout_limit ADD COLUMN created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_limit ADD COLUMN updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_limit ADD PRIMARY KEY (id);

CREATE INDEX idx_payout_limit_moc_id ON atlas_driver_offer_bpp.payout_limit USING btree (merchant_operating_city_id);
