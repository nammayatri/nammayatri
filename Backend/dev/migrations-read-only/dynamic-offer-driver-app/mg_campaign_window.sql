CREATE TABLE atlas_driver_offer_bpp.mg_campaign_window ();

ALTER TABLE atlas_driver_offer_bpp.mg_campaign_window ADD COLUMN campaign_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign_window ADD COLUMN end_time time without time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign_window ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign_window ADD COLUMN start_time time without time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign_window ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign_window ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign_window ADD PRIMARY KEY ( id);
