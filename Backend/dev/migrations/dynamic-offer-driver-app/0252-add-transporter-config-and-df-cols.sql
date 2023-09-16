ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_fee_calculation_time bigint; -- this gets added to endTime of window
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cgst_percentage double precision;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN sgst_percentage double precision;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_fee_calculator_batch_size int;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_fee_calculator_batch_gap bigint;

-- NAMMA YATRI ----

UPDATE atlas_driver_offer_bpp.transporter_config SET driver_fee_calculation_time = 0 where merchant_id = 'favorit0-0000-0000-0000-00000favorit';
UPDATE atlas_driver_offer_bpp.transporter_config SET cgst_percentage = 0.09 where merchant_id = 'favorit0-0000-0000-0000-00000favorit';
UPDATE atlas_driver_offer_bpp.transporter_config SET sgst_percentage = 0.09 where merchant_id = 'favorit0-0000-0000-0000-00000favorit';
UPDATE atlas_driver_offer_bpp.transporter_config SET driver_fee_calculator_batch_size = 25 where merchant_id = 'favorit0-0000-0000-0000-00000favorit';
UPDATE atlas_driver_offer_bpp.transporter_config SET driver_fee_calculator_batch_gap = 5 where merchant_id = 'favorit0-0000-0000-0000-00000favorit';

ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN offer_id character varying(100); -- can this be character(36)?
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN plan_offer_title text;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN stage_updated_at timestamp with time zone;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN bill_number int;
ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN autopay_payment_stage


ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN payer_vpa text;