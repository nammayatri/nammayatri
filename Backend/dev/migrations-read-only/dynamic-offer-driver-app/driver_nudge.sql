CREATE TABLE atlas_driver_offer_bpp.driver_nudge ();

ALTER TABLE atlas_driver_offer_bpp.driver_nudge ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_nudge ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_nudge ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_nudge ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_nudge ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_nudge ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_nudge ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_nudge ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_nudge ADD COLUMN estimated_distance integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_nudge ADD COLUMN actual_distance integer ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_nudge ADD COLUMN destination json ;