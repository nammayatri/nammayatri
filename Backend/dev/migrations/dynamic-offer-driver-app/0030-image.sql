ALTER TABLE atlas_driver_offer_bpp._operating_city_t RENAME TO operating_city;

ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate DROP COLUMN image_s3_path;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate DROP COLUMN verification_try_count;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate DROP COLUMN permit_start;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate DROP COLUMN permit_number;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate DROP COLUMN vehicle_color;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate DROP COLUMN vehicle_model;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate DROP COLUMN idfy_request_id;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate DROP COLUMN idfy_response_dump;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate DROP COLUMN consent;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate DROP COLUMN consent_timestamp;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD COLUMN failed_rules text[][];
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ALTER COLUMN fitness_expiry set NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD CONSTRAINT unique_rc_id UNIQUE (certificate_number, fitness_expiry);
ALTER TABLE atlas_driver_offer_bpp.vehicle_registration_certificate ADD CONSTRAINT vehicle_registration_certificate_pkey PRIMARY KEY (id);

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

CREATE TABLE atlas_driver_offer_bpp.driver_rc_association
(
    id character(36) NOT NULL,
    driver_id character varying(36) NOT NULL,
    rc_id character varying(36) NOT NULL,
    associated_on timestamp with time zone NOT NULL,
    associated_till timestamp with time zone,
    consent boolean DEFAULT true NOT NULL,
    consent_timestamp timestamp with time zone NOT NULL,
    CONSTRAINT DriverRCAssociationT_rc_id_fkey FOREIGN KEY (rc_id) REFERENCES atlas_driver_offer_bpp.vehicle_registration_certificate(id),
    CONSTRAINT DriverRCAssociationT_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_driver_offer_bpp.person(id)
);

CREATE TABLE atlas_driver_offer_bpp.idfy_verification
(
    id character(36) NOT NULL,
    driver_id character varying(36) NOT NULL,
    request_id character varying(36) NOT NULL,
    doc_type character varying(36) NOT NULL,
    status character varying(20) NOT NULL,
    idfy_response character (2555),
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    CONSTRAINT unique_request_id UNIQUE (request_id),
    CONSTRAINT IdfyVerificationT_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_driver_offer_bpp.person(id)
);

ALTER TABLE atlas_driver_offer_bpp.driver_license DROP COLUMN image_s3_path;
ALTER TABLE atlas_driver_offer_bpp.driver_license DROP COLUMN verification_try_count;
ALTER TABLE atlas_driver_offer_bpp.driver_license DROP COLUMN license_start;
ALTER TABLE atlas_driver_offer_bpp.driver_license DROP COLUMN idfy_request_id;
ALTER TABLE atlas_driver_offer_bpp.driver_license DROP COLUMN idfy_response_dump;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN failed_rules text[][];
ALTER TABLE atlas_driver_offer_bpp.driver_license ALTER COLUMN license_expiry set NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD CONSTRAINT unique_number UNIQUE (license_number);
