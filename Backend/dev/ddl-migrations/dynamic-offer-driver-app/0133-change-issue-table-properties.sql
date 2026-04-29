
ALTER TABLE atlas_driver_offer_bpp.comment DROP COLUMN author;
ALTER TABLE atlas_driver_offer_bpp.comment ADD COLUMN author_id character(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.issue_report DROP COLUMN category;
ALTER TABLE atlas_driver_offer_bpp.issue_report ADD COLUMN category_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.issue_category(id);

ALTER TABLE atlas_driver_offer_bpp.issue_report DROP COLUMN option;
ALTER TABLE atlas_driver_offer_bpp.issue_report ADD COLUMN option_id character(36) REFERENCES atlas_driver_offer_bpp.issue_option(id);

ALTER TABLE atlas_driver_offer_bpp.issue_translation ALTER COLUMN id TYPE character (36);
ALTER TABLE atlas_driver_offer_bpp.issue_category ALTER COLUMN id TYPE character (36);
ALTER TABLE atlas_driver_offer_bpp.issue_option ALTER COLUMN id TYPE character (36);
ALTER TABLE atlas_driver_offer_bpp.issue_option ALTER COLUMN issue_category_id TYPE character (36);