CREATE TABLE atlas_driver_offer_bpp.beckn_config ();

ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN buyer_finder_fee text ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN collected_by text NOT NULL default 'BPP';
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN domain text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN gateway_url text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN payment_params_json text ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN registry_url text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN settlement_type text ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN settlement_window text ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN static_terms_url text ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN subscriber_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN subscriber_url text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN unique_key_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN vehicle_category text NOT NULL default 'CAB';
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN cancellation_fee_percentage integer ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN cancellation_fee_amount integer ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN on_select_ttl_sec integer ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN on_search_ttl_sec integer ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN on_init_ttl_sec integer ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN on_confirm_ttl_sec integer ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN on_track_ttl_sec integer ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN on_status_ttl_sec integer ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN on_cancel_ttl_sec integer ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN on_update_ttl_sec integer ;



------- SQL updates -------


--- Drop columns section begins. Please be careful while running ---
ALTER TABLE atlas_driver_offer_bpp.beckn_config DROP COLUMN cancellation_fee_percentage;
ALTER TABLE atlas_driver_offer_bpp.beckn_config DROP COLUMN cancellation_fee_amount;
--- Drop columns section ends ---



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN multimodal_on_search_ttl_sec integer ;