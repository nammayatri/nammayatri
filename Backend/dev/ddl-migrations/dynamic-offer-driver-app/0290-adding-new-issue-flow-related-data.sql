ALTER TABLE atlas_driver_offer_bpp.issue_report
ADD COLUMN person_id character(36);

CREATE TABLE atlas_driver_offer_bpp.issue_message (
  id character(36) NOT NULL PRIMARY KEY,
  option_id character(36) REFERENCES atlas_driver_offer_bpp.issue_option (id),
  category_id character(36) REFERENCES atlas_driver_offer_bpp.issue_category (id),
  message character varying(255) NOT NULL,
  label text,
  priority int NOT NULL
);

CREATE TABLE atlas_driver_offer_bpp.issue_config (
  id character(36) NOT NULL PRIMARY KEY,
  auto_mark_issue_closed_duration double precision,
  on_auto_mark_issue_cls_msgs text[],
  on_create_issue_msgs text[],
  on_issue_reopen_msgs text[],
  on_kapt_mark_issue_res_msgs text[]
);

ALTER TABLE atlas_driver_offer_bpp.issue_option
ADD COLUMN priority int NOT NULL DEFAULT 1;

ALTER TABLE atlas_driver_offer_bpp.issue_report
ADD CONSTRAINT fk_person
FOREIGN KEY (person_id)
REFERENCES atlas_driver_offer_bpp.person(id);

ALTER TABLE atlas_driver_offer_bpp.issue_option ADD COLUMN label text;

ALTER TABLE atlas_driver_offer_bpp.issue_report ADD COLUMN chats text[];

ALTER TABLE atlas_driver_offer_bpp.issue_option ADD COLUMN issue_message_id character varying(255);