CREATE TABLE atlas_app.rating (
id character(36) NOT NULL,
ride_id character varying(36) NOT NULL REFERENCES atlas_app.ride (id),
rating_value int NOT NULL,
feedback_details character varying(255),
rider_id character varying(36) NOT NULL REFERENCES atlas_app.person (id),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_rating_primary PRIMARY KEY (ride_id)
,CONSTRAINT  rating_product_instance_id_key UNIQUE (ride_id)
);
ALTER TABLE atlas_app.rating OWNER TO atlas_app_user;