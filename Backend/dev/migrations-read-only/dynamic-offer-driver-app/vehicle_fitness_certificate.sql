CREATE TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ();

ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN application_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN application_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN category_of_vehicle text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN document_image_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN fitness_expiry timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN inspecting_authority text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN inspecting_on timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN next_inspection_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN rc_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN receipt_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN verification_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_fitness_certificate ADD COLUMN driver_id character varying(36) NOT NULL;