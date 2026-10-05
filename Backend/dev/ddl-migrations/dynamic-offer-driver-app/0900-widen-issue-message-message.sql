-- Driver issue_message.message was varchar(255); master has messages up to 666 chars (master sync failed). text removes the limit.
ALTER TABLE atlas_driver_offer_bpp.issue_message ALTER COLUMN message TYPE text;
