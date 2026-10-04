-- Rider side was widened in rider-app/1187; driver side stayed varchar(255), but master has driver issue messages up to 666 chars (master sync fails).
ALTER TABLE atlas_driver_offer_bpp.issue_message ALTER COLUMN message TYPE character varying(1000);
