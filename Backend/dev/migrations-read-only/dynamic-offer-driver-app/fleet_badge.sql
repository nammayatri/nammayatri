CREATE TABLE atlas_driver_offer_bpp.fleet_badge ();

ALTER TABLE atlas_driver_offer_bpp.fleet_badge ADD COLUMN badge_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge ADD COLUMN fleet_owner_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_badge ADD COLUMN person_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge ADD COLUMN badge_type text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_badge ALTER COLUMN badge_type SET DEFAULT 'DRIVER';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_badge ALTER COLUMN badge_type SET NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_badge ADD COLUMN badge_rank text ;