CREATE TABLE atlas_driver_offer_bpp.special_location (
id character(36) NOT NULL PRIMARY KEY,
location_name character varying(255) NOT NULL,
category character varying(255) NOT NULL,
gates text[] NOT NULL,
geom public.geometry(MultiPolygon),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_driver_offer_bpp.tag_category_mapping (
id character(36) NOT NULL,
tag character varying(255) NOT NULL PRIMARY KEY,
category character varying(255) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
