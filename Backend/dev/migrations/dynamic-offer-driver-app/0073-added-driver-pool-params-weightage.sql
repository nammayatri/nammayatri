ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cancellation_ratio_weightage int;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN acceptance_ratio_weightage int;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN availability_time_weightage int;
UPDATE atlas_driver_offer_bpp.transporter_config SET cancellation_ratio_weightage=-40 , acceptance_ratio_weightage=40, availability_time_weightage=70;
