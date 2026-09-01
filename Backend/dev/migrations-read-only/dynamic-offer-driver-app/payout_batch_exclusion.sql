CREATE TABLE atlas_driver_offer_bpp.payout_batch_exclusion ();

ALTER TABLE atlas_driver_offer_bpp.payout_batch_exclusion ADD COLUMN balance_at_evaluation double precision ;
ALTER TABLE atlas_driver_offer_bpp.payout_batch_exclusion ADD COLUMN beneficiary_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_batch_exclusion ADD COLUMN beneficiary_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_batch_exclusion ADD COLUMN corrected_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.payout_batch_exclusion ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_batch_exclusion ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_batch_exclusion ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_batch_exclusion ADD COLUMN notified_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.payout_batch_exclusion ADD COLUMN reason text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_batch_exclusion ADD COLUMN run_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_batch_exclusion ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_batch_exclusion ADD PRIMARY KEY ( id);
