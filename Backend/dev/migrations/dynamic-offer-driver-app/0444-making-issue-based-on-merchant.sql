ALTER TABLE atlas_driver_offer_bpp.issue_category
ADD COLUMN merchant_id text NOT NULL DEFAULT 'favorit0-0000-0000-0000-00000favorit';

ALTER TABLE atlas_driver_offer_bpp.issue_config
ADD COLUMN merchant_id text NOT NULL DEFAULT 'favorit0-0000-0000-0000-00000favorit';

ALTER TABLE atlas_driver_offer_bpp.issue_message
ADD COLUMN merchant_id text NOT NULL DEFAULT 'favorit0-0000-0000-0000-00000favorit';

ALTER TABLE atlas_driver_offer_bpp.issue_option
ADD COLUMN merchant_id text NOT NULL DEFAULT 'favorit0-0000-0000-0000-00000favorit';

ALTER TABLE atlas_driver_offer_bpp.issue_report
ADD COLUMN merchant_id text NOT NULL DEFAULT 'favorit0-0000-0000-0000-00000favorit';

ALTER TABLE atlas_driver_offer_bpp.issue_translation
ADD COLUMN merchant_id text NOT NULL DEFAULT 'favorit0-0000-0000-0000-00000favorit';

ALTER TABLE atlas_driver_offer_bpp.comment
ADD COLUMN merchant_id text NOT NULL DEFAULT 'favorit0-0000-0000-0000-00000favorit';