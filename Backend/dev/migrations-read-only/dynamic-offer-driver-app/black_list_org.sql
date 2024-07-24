CREATE TABLE atlas_driver_offer_bpp.black_list_org ();

ALTER TABLE atlas_driver_offer_bpp.black_list_org ADD COLUMN domain character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.black_list_org ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.black_list_org ADD COLUMN subscriber_id character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.black_list_org ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.black_list_org ADD COLUMN merchant_operating_city_id character varying(36)  default '';
ALTER TABLE atlas_driver_offer_bpp.black_list_org ADD COLUMN merchant_id character varying(36)  default '';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.black_list_org ALTER COLUMN merchant_operating_city_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.black_list_org ALTER COLUMN merchant_id SET NOT NULL;