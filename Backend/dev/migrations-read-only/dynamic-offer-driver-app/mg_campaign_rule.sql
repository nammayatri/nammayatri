CREATE TABLE atlas_driver_offer_bpp.mg_campaign_rule ();

ALTER TABLE atlas_driver_offer_bpp.mg_campaign_rule ADD COLUMN campaign_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign_rule ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign_rule ADD COLUMN metric text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign_rule ADD COLUMN params text ;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign_rule ADD COLUMN slabs text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign_rule ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign_rule ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign_rule ADD PRIMARY KEY ( id);
