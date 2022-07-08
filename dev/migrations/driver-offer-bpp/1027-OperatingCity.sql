CREATE TABLE IF NOT EXISTS atlas_transporter.operating_city
(
    id character(36) COLLATE pg_catalog."default" NOT NULL,
    organizationId character varying(255) COLLATE pg_catalog."default" NOT NULL,
    cityName character varying(255) COLLATE pg_catalog."default",
    enabled BOOLEAN NOT NULL,
    createdAt timestamp with time zone NOT NULL,
    updatedAt timestamp with time zone NOT NULL
    ,CONSTRAINT  OperatingCity_org_id_fkey FOREIGN KEY (organizationId) REFERENCES atlas_transporter.organization(id)
);

-- INSERT INTO atlas_transporter.OperatingCity (
--     id,
--     organizationId,
--     cityName,
--     enabled,
--     createdAt,
--     updatedAt
-- )
-- VALUES 
--     ('ASDA','ASDA1','BANGALORE',TRUE,current_timestamp,current_timestamp)