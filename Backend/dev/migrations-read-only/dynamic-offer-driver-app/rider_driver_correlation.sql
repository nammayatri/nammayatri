CREATE TABLE atlas_driver_offer_bpp.rider_driver_correlation ();

ALTER TABLE atlas_driver_offer_bpp.rider_driver_correlation ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.rider_driver_correlation ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_driver_correlation ADD COLUMN favourite boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.rider_driver_correlation ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_driver_correlation ADD COLUMN mobile_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_driver_correlation ADD COLUMN mobile_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_driver_correlation ADD COLUMN rider_detail_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_driver_correlation ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.rider_driver_correlation ADD PRIMARY KEY ( driver_id, rider_detail_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.rider_driver_correlation ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;