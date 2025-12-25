-- NAMMA YATRI ----

UPDATE atlas_driver_offer_bpp.plan SET cgst_percentage = 0.09 where merchant_id = 'favorit0-0000-0000-0000-00000favorit';
UPDATE atlas_driver_offer_bpp.plan SET sgst_percentage = 0.09 where merchant_id = 'favorit0-0000-0000-0000-00000favorit';

UPDATE atlas_driver_offer_bpp.transporter_config SET driver_fee_calculation_time = 0 where merchant_id = 'favorit0-0000-0000-0000-00000favorit';
UPDATE atlas_driver_offer_bpp.transporter_config SET driver_fee_calculator_batch_size = 25 where merchant_id = 'favorit0-0000-0000-0000-00000favorit';
UPDATE atlas_driver_offer_bpp.transporter_config SET driver_fee_calculator_batch_gap = 5 where merchant_id = 'favorit0-0000-0000-0000-00000favorit';

