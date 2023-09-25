CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.volunteer
(   id character(36) NOT NULL PRIMARY KEY,
    place character varying(255) NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

INSERT INTO atlas_driver_offer_bpp.volunteer VALUES('0ad46579-e881-4576-bc1e-c9a5b38fe785','Kolkata Railway Station',now(),now());