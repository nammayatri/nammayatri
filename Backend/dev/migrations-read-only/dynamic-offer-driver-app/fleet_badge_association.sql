CREATE TABLE atlas_driver_offer_bpp.fleet_badge_association ();

ALTER TABLE atlas_driver_offer_bpp.fleet_badge_association ADD COLUMN associated_on timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge_association ADD COLUMN associated_till timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge_association ADD COLUMN badge_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge_association ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge_association ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge_association ADD COLUMN fleet_owner_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge_association ADD COLUMN id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge_association ADD COLUMN is_active boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge_association ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_badge_association ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_badge_association ADD COLUMN badge_type text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_badge_association ALTER COLUMN badge_type SET DEFAULT 'DRIVER';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_badge_association ALTER COLUMN badge_type SET NOT NULL;