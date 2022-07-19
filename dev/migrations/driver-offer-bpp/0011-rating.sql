CREATE TABLE atlas_driver_offer_bpp.rating (
id character(36) NOT NULL,
ride_id character varying(36) NOT NULL,
rating_value bigint NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
driver_id character(36) NOT NULL
,CONSTRAINT  idx_rating_primary PRIMARY KEY (id)
,CONSTRAINT  rating_product_instance_id_key UNIQUE (ride_id)
);
ALTER TABLE atlas_driver_offer_bpp.rating OWNER TO atlas_driver_offer_bpp_user;
