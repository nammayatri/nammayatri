ALTER TABLE atlas_driver_offer_bpp.issue_config ADD COLUMN on_issue_close_msgs text[] DEFAULT '{}';
ALTER TABLE atlas_driver_offer_bpp.issue_config ADD COLUMN reopen_count int DEFAULT 0;
ALTER TABLE atlas_driver_offer_bpp.issue_report ADD COLUMN reopened_count int;