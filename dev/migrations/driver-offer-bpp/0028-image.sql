ALTER TABLE atlas_driver_offer_bpp._operating_city_t RENAME TO operating_city;

CREATE TABLE atlas_driver_offer_bpp.image
(
    id character(36) NOT NULL,
    person_id character varying(36) NOT NULL,
    organization_id character varying(36) NOT NULL,
    s3_path character varying(255) NOT NULL,
    image_type character varying(36) NOT NULL,
    is_valid boolean NOT NULL,
    created_at timestamp with time zone NOT NULL,
    CONSTRAINT ImageT_org_id_fkey FOREIGN KEY (organization_id) REFERENCES atlas_driver_offer_bpp.organization(id),
    CONSTRAINT ImageT_person_id_fkey FOREIGN KEY (person_id) REFERENCES atlas_driver_offer_bpp.person(id)
);


ALTER TABLE atlas_driver_offer_bpp.driver_license DROP COLUMN image_s3_path;
ALTER TABLE atlas_driver_offer_bpp.driver_license DROP COLUMN verification_try_count;

ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate DROP COLUMN image_s3_path;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate DROP COLUMN verification_try_count;


ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN version int8 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN active boolean NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD COLUMN version int8 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD COLUMN active boolean NOT NULL;