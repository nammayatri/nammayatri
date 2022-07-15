CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp._operating_city_t
(
    id character(36) COLLATE pg_catalog."default" NOT NULL,
    organization_id character varying(255) COLLATE pg_catalog."default" NOT NULL,
    city_name character varying(255) COLLATE pg_catalog."default",
    enabled boolean NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
    ,CONSTRAINT  OperatingCityT_org_id_fkey FOREIGN KEY (organization_id) REFERENCES atlas_driver_offer_bpp.organization(id)
);

-- local testing data
INSERT INTO atlas_driver_offer_bpp._operating_city_t (id, organization_id, city_name, enabled, created_at, updated_at) VALUES
  ('149c5260-f475-4ed7-849b-daba37751bbe','7f7896dd-787e-4a0b-8675-e9e6fe93bb8f','Bangalore', true, now(), now());
INSERT INTO atlas_driver_offer_bpp._operating_city_t (id, organization_id, city_name, enabled, created_at, updated_at) VALUES
  ('d4cb5e58-d655-476f-bf34-5863a4377016','7f7896dd-787e-4a0b-8675-c3f6fe93aa9e','Bangalore', true, now(), now());