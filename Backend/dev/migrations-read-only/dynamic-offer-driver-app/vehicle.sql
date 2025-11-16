CREATE TABLE atlas_driver_offer_bpp.vehicle ();

ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN capacity integer ;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN category text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN color text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN energy_type text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN make text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN model text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN registration_category text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN registration_no text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN size text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN variant text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN vehicle_class text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN vehicle_name text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD PRIMARY KEY ( driver_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN vehicle_rating double precision ;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN luggage_capacity integer ;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN air_conditioned boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN selected_service_tiers text[] NOT NULL default '{}';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN ventilator boolean ;
ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN oxygen boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN m_y_manufacturing date;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN downgrade_reason text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN vehicle_tags text[] ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN rule_based_upgrade_tiers json ;