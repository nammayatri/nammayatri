ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN delay INTEGER;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN contact_support_number text;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN toast_message text;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN secondary_actions text;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN social_media_links json;

-- update atlas_driver_offer_bpp.merchant_overlay set social_media_links = json_build_array(json_build_object ('prefixImage', null, 'suffixImage', null, 'link', 'https://youtube.com/shorts/x9cJN78j9V8?feature=shared', 'linkText', null, 'height', null, 'width', null)) ;

update atlas_driver_offer_bpp.merchant_overlay set delay = 0;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_fee_overlay_sending_time_limit_in_days INTEGER DEFAULT 15 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN overlay_batch_size INTEGER DEFAULT 50 NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN overlay_sent boolean DEFAULT false NOT NULL;
