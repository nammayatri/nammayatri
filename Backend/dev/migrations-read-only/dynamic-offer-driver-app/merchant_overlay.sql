CREATE TABLE atlas_driver_offer_bpp.merchant_overlay ();

ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN actions text[] NOT NULL default '{}';
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN actions2 json NOT NULL default json_build_array();
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN cancel_button_text text ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN contact_support_number text ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN delay integer ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN description text ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN end_point text ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN image_url text ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN language character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN link text ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN method text ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN ok_button_text text ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN overlay_key character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN req_body json NOT NULL default json_build_object();
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN secondary_actions text[] ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN secondary_actions2 json ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN show_push_notification boolean ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN social_media_links json ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN title text ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN toast_message text ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN udf1 character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ADD COLUMN vehicle_category text ;