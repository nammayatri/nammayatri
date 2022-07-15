CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp._operating_city_t
(
    id character(36) COLLATE pg_catalog."default" NOT NULL,
    organization_id character varying(255) COLLATE pg_catalog."default" NOT NULL,
    city_name character varying(255) COLLATE pg_catalog."default",
    enabled character varying(255) NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
    ,CONSTRAINT  OperatingCityT_org_id_fkey FOREIGN KEY (organization_id) REFERENCES atlas_driver_offer_bpp.organization(id)
);
